// File path: internal/workflow/runner.go
package workflow

import (
	"archive/zip"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/nicodishanthj/Katral_phase1/internal/common"
	ctxbuilder "github.com/nicodishanthj/Katral_phase1/internal/context"
	"github.com/nicodishanthj/Katral_phase1/internal/kb"
)

func (m *Manager) runWorkflow(ctx context.Context, projectID string, req Request) {
	switch req.kind {
	case KindKnowledgeBase:
		m.runKnowledgeBaseWorkflow(ctx, projectID, req)
	case KindDocumentGeneration:
		m.runDocumentGenerationWorkflow(ctx, projectID, req)
	default:
		m.runCodeConversionWorkflow(ctx, projectID, req)
	}
}

func (m *Manager) runKnowledgeBaseWorkflow(ctx context.Context, projectID string, req Request) {
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	ingestReq := IngestRequest{Repo: req.Mainframe, Stacks: req.Stacks, Collection: req.Collection, ProjectID: projectID}
	m.setWorkflowStep(projectID, 0, StepRunning, "Indexing knowledge base sources")
	resp, err := m.Ingest(ctx, ingestReq, req.ResetStore)
	if err != nil {
		if isCanceledErr(err) {
			m.markWorkflowCanceled(projectID, err)
		} else {
			m.failWorkflow(projectID, 0, err)
		}
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	message := fmt.Sprintf("Indexed %d documents", resp.Count)
	if resp.Warning != "" {
		message += fmt.Sprintf(" (%s)", resp.Warning)
	}
	m.setWorkflowStep(projectID, 0, StepCompleted, message)
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, 1, StepRunning, "Validating knowledge base coverage")
	summary, err := m.validateKnowledgeBase(ctx, projectID)
	if err != nil {
		if isCanceledErr(err) {
			m.markWorkflowCanceled(projectID, err)
		} else {
			m.failWorkflow(projectID, 1, err)
		}
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, 1, StepCompleted, summary)
	m.completeWorkflow(projectID)
}

func (m *Manager) runDocumentGenerationWorkflow(ctx context.Context, projectID string, req Request) {
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	if details := formatTargetConfigDetails(req.TargetConfig); details != "" {
		m.AppendLog("info", "Document generation target configuration: %s", details)
	}
	ingestReq := IngestRequest{Repo: req.Mainframe, Stacks: req.Stacks, Collection: req.Collection, ProjectID: projectID}
	m.setWorkflowStep(projectID, 0, StepRunning, appendTargetDetails("Indexing source material", req.TargetConfig))
	resp, err := m.Ingest(ctx, ingestReq, req.ResetStore)
	if err != nil {
		if isCanceledErr(err) {
			m.markWorkflowCanceled(projectID, err)
		} else {
			m.failWorkflow(projectID, 0, err)
		}
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	message := appendTargetDetails(fmt.Sprintf("Indexed %d documents", resp.Count), req.TargetConfig)
	if resp.Warning != "" {
		message += fmt.Sprintf(" (%s)", resp.Warning)
	}
	m.setWorkflowStep(projectID, 0, StepCompleted, message)
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, 1, StepRunning, appendTargetDetails("Generating documentation package", req.TargetConfig))
	summary, err := m.summarizeDocumentation(ctx, projectID)
	if err != nil {
		if isCanceledErr(err) {
			m.markWorkflowCanceled(projectID, err)
		} else {
			m.failWorkflow(projectID, 1, err)
		}
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, 1, StepCompleted, appendTargetDetails(summary, req.TargetConfig))
	m.completeWorkflow(projectID)
}

func (m *Manager) runCodeConversionWorkflow(ctx context.Context, projectID string, req Request) {
	const (
		ingestionStep  = 0
		generationStep = 1
		packageStep    = 2
		reviewStep     = 3
	)
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	if details := formatTargetConfigDetails(req.TargetConfig); details != "" {
		m.AppendLog("info", "Code conversion target configuration: %s", details)
	}
	ingestReq := IngestRequest{Repo: req.Mainframe, Stacks: req.Stacks, Collection: req.Collection, ProjectID: projectID}
	m.setWorkflowStep(projectID, ingestionStep, StepRunning, appendTargetDetails("Indexing mainframe ecosystem", req.TargetConfig))
	resp, err := m.Ingest(ctx, ingestReq, req.ResetStore)
	if err != nil {
		if isCanceledErr(err) {
			m.markWorkflowCanceled(projectID, err)
		} else {
			m.failWorkflow(projectID, ingestionStep, err)
		}
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	message := appendTargetDetails(fmt.Sprintf("Indexed %d documents", resp.Count), req.TargetConfig)
	if resp.Warning != "" {
		message += fmt.Sprintf(" (%s)", resp.Warning)
	}
	m.setWorkflowStep(projectID, ingestionStep, StepCompleted, message)
	generatorCommand := strings.TrimSpace(req.GeneratorCommand)
	springProvided := strings.TrimSpace(req.SpringProject) != ""
	needsGeneration := generatorCommand != "" || req.VerifySpring || (!springProvided && m.metadata != nil)
	if !needsGeneration {
		m.setWorkflowStep(projectID, generationStep, StepSkipped, appendTargetDetails("No generation tasks requested", req.TargetConfig))
	} else {
		if m.workflowCanceled(ctx, projectID) {
			return
		}
		m.setWorkflowStep(projectID, generationStep, StepRunning, appendTargetDetails("Running code generation tasks", req.TargetConfig))
		if generatorCommand != "" {
			output, err := m.executeGeneratorCommand(ctx, req)
			if err != nil {
				if isCanceledErr(err) {
					m.markWorkflowCanceled(projectID, err)
				} else {
					m.failWorkflow(projectID, generationStep, err)
				}
				return
			}
			if strings.TrimSpace(output) != "" {
				m.AppendLog("info", output)
			}
		} else {
			path, err := m.generateSpringProject(ctx, projectID, req)
			if err != nil {
				if isCanceledErr(err) {
					m.markWorkflowCanceled(projectID, err)
				} else {
					m.failWorkflow(projectID, generationStep, err)
				}
				return
			}
			if strings.TrimSpace(path) != "" {
				req.SpringProject = path
				springProvided = true
			}
		}
		if strings.TrimSpace(req.SpringProject) != "" {
			springProvided = true
		}
		verifySpring := req.VerifySpring && springProvided
		if verifySpring {
			if err := m.verifySpringProject(ctx, req); err != nil {
				if isCanceledErr(err) {
					m.markWorkflowCanceled(projectID, err)
				} else {
					m.failWorkflow(projectID, generationStep, err)
				}
				return
			}
		} else if req.VerifySpring {
			m.AppendLog("warn", "Spring verification requested but --spring path missing")
		}
		if m.workflowCanceled(ctx, projectID) {
			return
		}
		m.setWorkflowStep(projectID, generationStep, StepCompleted, appendTargetDetails("Generation tasks complete", req.TargetConfig))
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	packageStepMessage := appendTargetDetails("Packaging conversion deliverables", req.TargetConfig)
	m.setWorkflowStep(projectID, packageStep, StepRunning, packageStepMessage)
	deliverables := make([]string, 0, 2)
	if springProvided {
		artifactPath, err := m.packageSpringProject(ctx, projectID, req.SpringProject)
		if err != nil {
			if isCanceledErr(err) {
				m.markWorkflowCanceled(projectID, err)
			} else {
				m.failWorkflow(projectID, packageStep, err)
			}
			return
		}
		m.setSpringArtifact(projectID, artifactPath)
		deliverables = append(deliverables, fmt.Sprintf("Spring project packaged: %s", filepath.Base(artifactPath)))
	}
	var conversionArtifacts map[string]string
	var conversionErr error
	if m.store != nil {
		docs, err := m.store.AllDocs(ctx, projectID)
		if err != nil {
			conversionErr = fmt.Errorf("load conversion docs: %w", err)
		} else {
			conversionArtifacts, conversionErr = m.packageConversion(ctx, projectID, docs)
		}
	}
	if conversionErr != nil {
		if isCanceledErr(conversionErr) {
			m.markWorkflowCanceled(projectID, conversionErr)
		} else {
			m.failWorkflow(projectID, packageStep, conversionErr)
		}
		return
	}
	m.setConversionArtifacts(projectID, conversionArtifacts)
	if len(conversionArtifacts) > 0 {
		kinds := make([]string, 0, len(conversionArtifacts))
		for kind := range conversionArtifacts {
			kinds = append(kinds, kind)
		}
		sort.Strings(kinds)
		deliverables = append(deliverables, fmt.Sprintf("Conversion packages ready: %s", strings.Join(kinds, ", ")))
	}
	if len(deliverables) == 0 {
		skipped := appendTargetDetails("No Spring project or conversion artifacts available for packaging", req.TargetConfig)
		m.setWorkflowStep(projectID, packageStep, StepSkipped, skipped)
	} else {
		completed := appendTargetDetails(strings.Join(deliverables, "; "), req.TargetConfig)
		m.setWorkflowStep(projectID, packageStep, StepCompleted, completed)
	}
	if req.ReviewPrompt == "" {
		m.setWorkflowStep(projectID, reviewStep, StepSkipped, "No review prompt provided")
		m.completeWorkflow(projectID)
		return
	}
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, reviewStep, StepRunning, "Reviewing generated services")
	answer, refs := m.reviewKnowledge(req.ReviewPrompt)
	m.setWorkflowReview(projectID, answer, refs)
	if m.workflowCanceled(ctx, projectID) {
		return
	}
	m.setWorkflowStep(projectID, reviewStep, StepCompleted, "Review complete")
	m.completeWorkflow(projectID)
}

func (m *Manager) validateKnowledgeBase(ctx context.Context, projectID string) (string, error) {
	docs, err := m.store.AllDocs(ctx, projectID)
	if err != nil {
		return "", err
	}
	if len(docs) == 0 {
		return "", fmt.Errorf("knowledge base is empty after ingest")
	}
	techs := make(map[string]struct{})
	types := make(map[string]struct{})
	for _, doc := range docs {
		for _, tech := range doc.Technologies {
			trimmed := strings.ToLower(strings.TrimSpace(tech))
			if trimmed != "" {
				techs[trimmed] = struct{}{}
			}
		}
		if doc.Type != "" {
			types[strings.ToLower(strings.TrimSpace(doc.Type))] = struct{}{}
		}
	}
	summaryParts := []string{fmt.Sprintf("%d documents indexed", len(docs))}
	if len(techs) > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d technology tags", len(techs)))
	}
	if len(types) > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d document types", len(types)))
	}
	summary := strings.Join(summaryParts, ", ")
	m.AppendLog("info", "Knowledge base validation complete: %s", summary)
	return fmt.Sprintf("Knowledge base ready (%s)", summary), nil
}

func (m *Manager) summarizeDocumentation(ctx context.Context, projectID string) (string, error) {
	docs, err := m.store.AllDocs(ctx, projectID)
	if err != nil {
		return "", err
	}
	if len(docs) == 0 {
		return "", fmt.Errorf("knowledge base is empty; ingest source assets first")
	}
	enrichedDocs, changed, err := m.enrichDocumentation(ctx, projectID, docs)
	if err != nil {
		return "", err
	}
	if changed {
		if err := m.store.ReplaceDocs(ctx, projectID, enrichedDocs); err != nil {
			return "", err
		}
		if err := m.refreshRetriever(ctx, projectID); err != nil {
			return "", err
		}
		docs = enrichedDocs
	} else {
		docs = enrichedDocs
	}
	var programs, flows, businessRules, workingNotes, modernizations int
	var docSummaries, crossRefs, impactAnalyses int
	var flowPrompts, businessPrompts, functionalPrompts, technicalPrompts int
	var migrationAssessments, componentMappings, apiMatrices, migrationTimelines int
	for _, doc := range docs {
		switch doc.Type {
		case "metadata":
			programs++
		case "flow", "cics_flow", "mq_flow":
			flows++
		case "business_rules":
			businessRules++
		case "working_notes":
			workingNotes++
		case "modernization":
			modernizations++
		case docTypeSummary:
			docSummaries++
		case docTypeCrossReference:
			crossRefs++
		case docTypeImpact:
			impactAnalyses++
		case docTypeProgramFlow:
			flowPrompts++
		case docTypeBusinessPrompt:
			businessPrompts++
		case docTypeFunctionalSpec:
			functionalPrompts++
		case docTypeTechnicalSpec:
			technicalPrompts++
		case docTypeMigrationAssessment:
			migrationAssessments++
		case docTypeComponentMapping:
			componentMappings++
		case docTypeAPICompatibility:
			apiMatrices++
		case docTypeMigrationTimeline:
			migrationTimelines++
		}
	}
	summaryParts := make([]string, 0, 5)
	if programs > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d program briefs", programs))
	}
	if flows > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d flow guides", flows))
	}
	if workingNotes > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d working note collections", workingNotes))
	}
	if businessRules > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d business rule reports", businessRules))
	}
	if modernizations > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d modernization blueprints", modernizations))
	}
	if docSummaries > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d documentation summaries", docSummaries))
	}
	if crossRefs > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d cross-reference maps", crossRefs))
	}
	if impactAnalyses > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d impact assessments", impactAnalyses))
	}
	if flowPrompts > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d program flow prompts", flowPrompts))
	}
	if businessPrompts > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d business rule prompts", businessPrompts))
	}
	if functionalPrompts > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d functional specification prompts", functionalPrompts))
	}
	if technicalPrompts > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d technical specification prompts", technicalPrompts))
	}
	if migrationAssessments > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d migration assessment reports", migrationAssessments))
	}
	if componentMappings > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d component mapping guides", componentMappings))
	}
	if apiMatrices > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d API compatibility matrices", apiMatrices))
	}
	if migrationTimelines > 0 {
		summaryParts = append(summaryParts, fmt.Sprintf("%d migration timeline roadmaps", migrationTimelines))
	}
	if len(summaryParts) == 0 {
		return "", fmt.Errorf("no documentation artifacts were generated")
	}
	summary := strings.Join(summaryParts, ", ")
	artifacts, err := m.packageDocumentation(ctx, projectID, docs)
	if err != nil {
		return "", err
	}
	consolidatedPath, consolidatedErr := m.generateConsolidatedDocumentation(ctx, projectID, docs)
	if consolidatedErr != nil {
		m.AppendLog("warn", "Consolidated documentation generation failed for project %s: %v", projectID, consolidatedErr)
	} else {
		m.setConsolidatedDoc(projectID, consolidatedPath)
		if strings.TrimSpace(consolidatedPath) != "" {
			m.AppendLog("info", "Consolidated documentation prepared for project %s: %s", projectID, consolidatedPath)
		}
	}
	m.setDocumentationArtifacts(projectID, artifacts)
	m.AppendLog("info", "Documentation package synthesized: %s", summary)
	return fmt.Sprintf("Documentation package ready (%s). Access summaries via /api/project-doc.", summary), nil
}

func (m *Manager) reviewKnowledge(prompt string) (string, []Reference) {
	answer := m.retriever.Answer(prompt)
	refs := make([]Reference, 0, 3)
	if m.ctx != nil {
		req := ctxbuilder.GoalRequest{Query: prompt, Limit: 3, Embedder: m.provider}
		result, err := m.ctx.BuildGoalContext(context.Background(), req)
		if err != nil {
			common.Logger().Warn("workflow: context builder review failed", "error", err)
		} else {
			for _, snip := range result.Snippets {
				refs = append(refs, Reference{
					Program: snip.Program,
					Source:  snip.Source,
					Score:   snip.Score,
					Summary: snip.Summary,
					Graph:   snip.Graph,
				})
			}
		}
	}
	if len(refs) == 0 {
		results := m.retriever.Search(prompt, 3)
		for _, res := range results {
			refs = append(refs, Reference{
				Program: res.Doc.Program,
				Source:  res.Doc.SourcePath,
				Score:   res.Score,
				Summary: res.Doc.Summary,
				Graph:   res.Graph,
			})
		}
	}
	return answer, refs
}

func (m *Manager) setWorkflowReview(projectID string, answer string, refs []Reference) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if session.state.Status == "canceled" {
		m.workflowMu.Unlock()
		return
	}
	session.state.ReviewAnswer = answer
	if len(refs) > 0 {
		session.state.ReviewReferences = append([]Reference(nil), refs...)
	} else {
		session.state.ReviewReferences = nil
	}
	m.workflowMu.Unlock()
}

func (m *Manager) setSpringArtifact(projectID, path string) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	session.state.SpringArtifact = strings.TrimSpace(path)
	m.workflowMu.Unlock()
}

func (m *Manager) setConversionArtifacts(projectID string, artifacts map[string]string) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if len(artifacts) == 0 {
		session.state.ConversionArtifacts = nil
		m.workflowMu.Unlock()
		return
	}
	clean := make(map[string]string, len(artifacts))
	for key, value := range artifacts {
		trimmedKey := strings.ToLower(strings.TrimSpace(key))
		trimmedValue := strings.TrimSpace(value)
		if trimmedKey == "" || trimmedValue == "" {
			continue
		}
		clean[trimmedKey] = trimmedValue
	}
	if len(clean) == 0 {
		session.state.ConversionArtifacts = nil
		m.workflowMu.Unlock()
		return
	}
	session.state.ConversionArtifacts = clean
	m.workflowMu.Unlock()
}

func (m *Manager) setConsolidatedDoc(projectID, path string) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	session.state.ConsolidatedDoc = strings.TrimSpace(path)
	m.workflowMu.Unlock()
}

func (m *Manager) setDocumentationArtifacts(projectID string, artifacts map[string]string) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if len(artifacts) == 0 {
		session.state.DocumentationArtifacts = nil
		m.workflowMu.Unlock()
		return
	}
	clean := make(map[string]string, len(artifacts))
	for key, value := range artifacts {
		trimmedKey := strings.ToLower(strings.TrimSpace(key))
		trimmedValue := strings.TrimSpace(value)
		if trimmedKey == "" || trimmedValue == "" {
			continue
		}
		clean[trimmedKey] = trimmedValue
	}
	if len(clean) == 0 {
		session.state.DocumentationArtifacts = nil
		m.workflowMu.Unlock()
		return
	}
	session.state.DocumentationArtifacts = clean
	m.workflowMu.Unlock()
}

func (m *Manager) packageSpringProject(ctx context.Context, projectID, sourceDir string) (string, error) {
	sourceDir = strings.TrimSpace(sourceDir)
	if sourceDir == "" {
		return "", fmt.Errorf("spring project path required")
	}
	info, err := os.Stat(sourceDir)
	if err != nil {
		return "", fmt.Errorf("locate spring project: %w", err)
	}
	if !info.IsDir() {
		return "", fmt.Errorf("spring project path %s is not a directory", sourceDir)
	}
	root, err := m.ensureArtifactRoot()
	if err != nil {
		return "", err
	}
	safeProject := safeFileComponent(projectID)
	timestamp := time.Now().UTC().Format("20060102T150405Z")
	artifactName := fmt.Sprintf("%s-%s.zip", safeProject, timestamp)
	finalPath := filepath.Join(root, artifactName)
	tempPath := finalPath + ".tmp"

	file, err := os.OpenFile(tempPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return "", fmt.Errorf("create artifact: %w", err)
	}
	zipWriter := zip.NewWriter(file)
	m.AppendLog("info", "Packaging Spring project from %s for project %s", sourceDir, projectID)
	walkErr := filepath.WalkDir(sourceDir, func(path string, d os.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}
		rel, err := filepath.Rel(sourceDir, path)
		if err != nil {
			return err
		}
		if rel == "." {
			if d.IsDir() {
				return nil
			}
		}
		rel = filepath.ToSlash(rel)
		if d.IsDir() {
			if rel != "." {
				if !strings.HasSuffix(rel, "/") {
					rel += "/"
				}
				_, err := zipWriter.Create(rel)
				return err
			}
			return nil
		}
		fileInfo, err := d.Info()
		if err != nil {
			return err
		}
		header, err := zip.FileInfoHeader(fileInfo)
		if err != nil {
			return err
		}
		header.Name = rel
		header.Method = zip.Deflate
		writer, err := zipWriter.CreateHeader(header)
		if err != nil {
			return err
		}
		inFile, err := os.Open(path)
		if err != nil {
			return err
		}
		_, copyErr := io.Copy(writer, inFile)
		closeErr := inFile.Close()
		if copyErr != nil {
			return copyErr
		}
		if closeErr != nil {
			return closeErr
		}
		return nil
	})
	if walkErr != nil {
		_ = zipWriter.Close()
		_ = file.Close()
		_ = os.Remove(tempPath)
		return "", fmt.Errorf("package spring project: %w", walkErr)
	}
	if err := zipWriter.Close(); err != nil {
		_ = file.Close()
		_ = os.Remove(tempPath)
		return "", fmt.Errorf("finalize artifact: %w", err)
	}
	if err := file.Close(); err != nil {
		_ = os.Remove(tempPath)
		return "", fmt.Errorf("close artifact: %w", err)
	}
	if err := os.Rename(tempPath, finalPath); err != nil {
		_ = os.Remove(tempPath)
		return "", fmt.Errorf("finalize artifact: %w", err)
	}
	absPath, err := filepath.Abs(finalPath)
	if err != nil {
		_ = os.Remove(finalPath)
		return "", fmt.Errorf("resolve artifact path: %w", err)
	}
	m.AppendLog("info", "Packaged Spring project for project %s: %s", projectID, absPath)
	return absPath, nil
}

func (m *Manager) ensureArtifactRoot() (string, error) {
	root := strings.TrimSpace(m.artifactRoot)
	if root == "" {
		root = filepath.Join(os.TempDir(), "katral_artifacts")
	}
	if err := os.MkdirAll(root, 0o755); err != nil {
		return "", fmt.Errorf("create artifact directory: %w", err)
	}
	if strings.TrimSpace(m.artifactRoot) == "" {
		m.workflowMu.Lock()
		if m.artifactRoot == "" {
			m.artifactRoot = root
		}
		m.workflowMu.Unlock()
	}
	return root, nil
}

func (m *Manager) packageDocumentation(ctx context.Context, projectID string, docs []kb.Doc) (map[string]string, error) {
	return m.packageDocFamilies(ctx, projectID, docs, "documentation_")
}

func (m *Manager) packageConversion(ctx context.Context, projectID string, docs []kb.Doc) (map[string]string, error) {
	return m.packageDocFamilies(ctx, projectID, docs, "conversion_")
}

func (m *Manager) packageDocFamilies(ctx context.Context, projectID string, docs []kb.Doc, prefix string) (map[string]string, error) {
	prefix = strings.ToLower(strings.TrimSpace(prefix))
	families := make(map[string][]kb.Doc)
	for _, doc := range docs {
		kind := strings.ToLower(strings.TrimSpace(doc.Type))
		if prefix != "" && !strings.HasPrefix(kind, prefix) {
			continue
		}
		families[kind] = append(families[kind], doc)
	}
	if len(families) == 0 {
		return nil, nil
	}
	root, err := m.ensureArtifactRoot()
	if err != nil {
		return nil, err
	}
	safeProject := safeFileComponent(projectID)
	timestamp := time.Now().UTC().Format("20060102T150405Z")
	generatedAt := time.Now().UTC()
	packaged := make(map[string]string, len(families))
	for kind, docs := range families {
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		default:
		}
		safeKind := safeFileComponent(kind)
		if safeKind == "" || safeKind == "project" {
			safeKind = "documentation"
		}
		artifactName := fmt.Sprintf("%s-%s-%s.zip", safeProject, safeKind, timestamp)
		finalPath := filepath.Join(root, artifactName)
		tempPath := finalPath + ".tmp"
		file, err := os.OpenFile(tempPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
		if err != nil {
			return nil, fmt.Errorf("create artifact: %w", err)
		}
		zipWriter := zip.NewWriter(file)
		cleanup := func() {
			_ = zipWriter.Close()
			_ = file.Close()
			_ = os.Remove(tempPath)
		}
		sorted := make([]kb.Doc, len(docs))
		copy(sorted, docs)
		sort.Slice(sorted, func(i, j int) bool {
			left := sorted[i]
			right := sorted[j]
			if !strings.EqualFold(strings.TrimSpace(left.Program), strings.TrimSpace(right.Program)) {
				return strings.ToUpper(strings.TrimSpace(left.Program)) < strings.ToUpper(strings.TrimSpace(right.Program))
			}
			return left.ID < right.ID
		})
		nameCounts := make(map[string]int)
		for idx, doc := range sorted {
			if err := ctx.Err(); err != nil {
				cleanup()
				return nil, err
			}
			programDir := safeFileComponent(doc.Program)
			if programDir == "" || programDir == "project" {
				programDir = "program"
			}
			baseName := safeFileComponent(doc.ID)
			if baseName == "" || baseName == "project" {
				baseName = fmt.Sprintf("doc-%d", idx+1)
			}
			key := programDir + "/" + baseName
			if count := nameCounts[key]; count > 0 {
				baseName = fmt.Sprintf("%s-%d", baseName, count+1)
			}
			nameCounts[key]++
			entryName := fmt.Sprintf("%s/%s.json", programDir, baseName)
			writer, err := zipWriter.Create(entryName)
			if err != nil {
				cleanup()
				return nil, fmt.Errorf("create archive entry: %w", err)
			}
			payload, err := json.MarshalIndent(doc, "", "  ")
			if err != nil {
				cleanup()
				return nil, fmt.Errorf("marshal document: %w", err)
			}
			if _, err := writer.Write(payload); err != nil {
				cleanup()
				return nil, fmt.Errorf("write document: %w", err)
			}
		}
		manifest := map[string]interface{}{
			"project_id":     projectID,
			"artifact_type":  kind,
			"generated_at":   generatedAt.Format(time.RFC3339Nano),
			"document_count": len(docs),
			"document_ids":   collectDocIDs(sorted),
		}
		manifestWriter, err := zipWriter.Create("manifest.json")
		if err != nil {
			cleanup()
			return nil, fmt.Errorf("create manifest: %w", err)
		}
		manifestPayload, err := json.MarshalIndent(manifest, "", "  ")
		if err != nil {
			cleanup()
			return nil, fmt.Errorf("marshal manifest: %w", err)
		}
		if _, err := manifestWriter.Write(manifestPayload); err != nil {
			cleanup()
			return nil, fmt.Errorf("write manifest: %w", err)
		}
		if err := zipWriter.Close(); err != nil {
			cleanup()
			return nil, fmt.Errorf("finalize artifact: %w", err)
		}
		if err := file.Close(); err != nil {
			cleanup()
			return nil, fmt.Errorf("close artifact: %w", err)
		}
		if err := os.Rename(tempPath, finalPath); err != nil {
			cleanup()
			return nil, fmt.Errorf("finalize artifact: %w", err)
		}
		absPath, err := filepath.Abs(finalPath)
		if err != nil {
			_ = os.Remove(finalPath)
			return nil, fmt.Errorf("resolve artifact path: %w", err)
		}
		packaged[kind] = absPath
		m.AppendLog("info", "Packaged %s artifact for project %s: %s", kind, projectID, absPath)
	}
	return packaged, nil
}

func collectDocIDs(docs []kb.Doc) []string {
	if len(docs) == 0 {
		return nil
	}
	ids := make([]string, 0, len(docs))
	for _, doc := range docs {
		if trimmed := strings.TrimSpace(doc.ID); trimmed != "" {
			ids = append(ids, trimmed)
		}
	}
	return ids
}

func safeFileComponent(name string) string {
	trimmed := strings.TrimSpace(name)
	if trimmed == "" {
		return "project"
	}
	var builder strings.Builder
	for _, r := range trimmed {
		switch {
		case r >= 'a' && r <= 'z':
			builder.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			builder.WriteRune(r + 32)
		case r >= '0' && r <= '9':
			builder.WriteRune(r)
		case r == '-', r == '_':
			builder.WriteRune('-')
		default:
			builder.WriteRune('-')
		}
	}
	sanitized := strings.Trim(builder.String(), "-")
	if sanitized == "" {
		return "project"
	}
	return sanitized
}

func (m *Manager) setWorkflowStep(projectID string, index int, status StepStatus, message string) {
	m.workflowMu.Lock()
	defer m.workflowMu.Unlock()
	session, ok := m.workflows[projectID]
	if !ok {
		return
	}
	if session.state.Status == "canceled" {
		return
	}
	if index < 0 || index >= len(session.state.Steps) {
		return
	}
	now := time.Now().UTC()
	step := &session.state.Steps[index]
	switch status {
	case StepRunning:
		step.StartedAt = &now
	case StepCompleted, StepSkipped, StepError:
		if step.StartedAt == nil {
			step.StartedAt = &now
		}
		step.CompletedAt = &now
	}
	step.Status = status
	if message != "" {
		step.Message = message
	}
}

func (m *Manager) failWorkflow(projectID string, index int, err error) {
	m.AppendLog("error", "Workflow failed for project %s: %v", projectID, err)
	m.setWorkflowStep(projectID, index, StepError, err.Error())
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if session.state.Status == "canceled" {
		m.workflowMu.Unlock()
		return
	}
	now := time.Now().UTC()
	session.state.Status = "error"
	session.state.Running = false
	session.state.CompletedAt = &now
	if err != nil {
		session.state.Error = err.Error()
	} else {
		session.state.Error = ""
	}
	session.cancel = nil
	snapshot := cloneState(session.state)
	m.workflowMu.Unlock()
	m.persistProjectState(projectID, snapshot)
}

func (m *Manager) completeWorkflow(projectID string) {
	m.AppendLog("info", "Workflow completed successfully for project %s", projectID)
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if session.state.Status == "canceled" {
		m.workflowMu.Unlock()
		return
	}
	now := time.Now().UTC()
	session.state.Status = "completed"
	session.state.Running = false
	session.state.CompletedAt = &now
	session.state.Error = ""
	session.cancel = nil
	snapshot := cloneState(session.state)
	m.workflowMu.Unlock()
	m.persistProjectState(projectID, snapshot)
}

func (m *Manager) workflowCanceled(ctx context.Context, projectID string) bool {
	select {
	case <-ctx.Done():
		m.markWorkflowCanceled(projectID, ctx.Err())
		return true
	default:
		return false
	}
}

func (m *Manager) markWorkflowCanceled(projectID string, cause error) {
	m.workflowMu.Lock()
	session, ok := m.workflows[projectID]
	if !ok {
		m.workflowMu.Unlock()
		return
	}
	if session.state.Status == "canceled" {
		m.workflowMu.Unlock()
		return
	}
	now := time.Now().UTC()
	session.state.Status = "canceled"
	session.state.Running = false
	session.state.CompletedAt = &now
	if cause != nil && !isCanceledErr(cause) {
		session.state.Error = cause.Error()
	} else {
		session.state.Error = ""
	}
	for i := range session.state.Steps {
		step := &session.state.Steps[i]
		if step.Status == StepRunning {
			if step.StartedAt == nil {
				step.StartedAt = &now
			}
			step.CompletedAt = &now
			step.Status = StepError
			if step.Message == "" {
				step.Message = "Canceled"
			}
			break
		}
	}
	cancel := session.cancel
	session.cancel = nil
	snapshot := cloneState(session.state)
	m.workflowMu.Unlock()
	if cancel != nil {
		cancel()
	}
	if cause != nil && !isCanceledErr(cause) {
		m.AppendLog("error", "Workflow canceled for project %s: %v", projectID, cause)
	} else {
		m.AppendLog("info", "Workflow canceled for project %s", projectID)
	}
	m.persistProjectState(projectID, snapshot)
}

func isCanceledErr(err error) bool {
	if err == nil {
		return false
	}
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}
