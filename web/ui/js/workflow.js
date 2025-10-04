import {
  postJSON,
  getJSON,
  escapeHtml,
  formatMultiline,
  formatTimestamp,
  populateDatalist
} from './utils.js';
import { renderMarkdown, maybeRenderMermaid, renderFlowDiagramBlock } from './markdown.js';
import { markStepComplete } from './tabs.js';
import { initConsolidatedDoc, checkConsolidatedDoc } from './consolidated_doc.js';
import {
  getSelectedFlow,
  getFlowConfig,
  getKnowledgeFlows,
  getModernizationFlows,
  getAngularMigrationFlows,
  getTargetPresetConfig,
  setProjectIdentifier,
  getProjectIdentifier,
  getMigrationConfig,
  onProjectIdentifierChange,
  onFlowChange
} from './state.js';

const flowConfig = getFlowConfig();
const knowledgeFlows = getKnowledgeFlows();
const modernizationFlows = getModernizationFlows();
const angularMigrationFlows = getAngularMigrationFlows();
const migrationConfigState = getMigrationConfig();
const artifactLabelMap = {
  spring: 'Spring Artifact',
  documentation_summary: 'Documentation Summaries',
  documentation_cross_reference: 'Cross-Reference Maps',
  documentation_impact_analysis: 'Impact Assessments',
  documentation_migration_assessment: 'Migration Assessment Reports',
  documentation_component_mapping: 'Component Mapping Guides',
  documentation_api_compatibility: 'API Compatibility Matrices',
  documentation_migration_timeline: 'Migration Timeline Roadmaps',
  documentation_migration_complexity: 'Migration Complexity Analyses',
  documentation_pattern_recommendations: 'Pattern Recommendation Guides',
  documentation_migrated_code_review: 'Migrated Component Code Reviews',
  documentation_performance_comparison: 'Performance Comparison Analyses',
  documentation_side_by_side_comparison: 'Side-by-Side Comparison Briefs',
  documentation_migration_script: 'Migration Script Blueprints',
  documentation_component_library_mapping: 'Component Library Mapping Guides',
  documentation_migration_validation_tests: 'Migration Validation Test Plans',
  conversion_summary: 'Conversion Summaries',
  conversion_mapping: 'Conversion Mappings',
  conversion_source: 'Conversion Sources'
};

function formatArtifactLabel(kind) {
  const key = String(kind || '').toLowerCase();
  if (artifactLabelMap[key]) {
    return artifactLabelMap[key];
  }
  if (!key) {
    return 'Artifact';
  }
  return key
    .replace(/[_\-]+/g, ' ')
    .split(' ')
    .map(part => (part ? part.charAt(0).toUpperCase() + part.slice(1) : ''))
    .join(' ')
    .trim() || 'Artifact';
}

let workflowProgressPane;
let workflowProgressSummary;
let workflowProgressList;
let workflowStacksDisplay;
let workflowStacksStatus;
let workflowProjectEl;
let workflowCollectionInput;
let workflowMainframeInput;
let workflowGeneratorInput;
let workflowGeneratorDirInput;
let workflowSpringInput;
let workflowReviewInput;
let workflowProjectOptions;
let workflowMainframeOptions;
let workflowCollectionOptions;
let workflowGeneratorOptions;
let workflowGeneratorDirOptions;
let workflowSpringOptions;
let workflowReviewEl;
let workflowStatusEl;
let workflowStepsEl;
let workflowDownloadContainer;
let workflowDownloadBtn;
let workflowDownloadStatus;
let workflowArtifactSelect;
let workflowStartBtn;
let workflowStopBtn;
let documentationArtifactsPane;
let documentationArtifactsStatusEl;
let documentationArtifactsListEl;
let recentProjectsCard;
let recentProjectsSelect;
let recentProjectInfo;
let runningWorkflowsCard;
let runningWorkflowsList;
let workflowFlowGroups;
let pendingDocWorkflowProjectId = null;
let workflowPollId = null;
let documentationArtifactsFetchController = null;
let documentationArtifactsProjectId = '';
let documentationArtifactsSignature = '';
let documentationArtifactsRunning = false;

function ensureDocumentationArtifactsVisible() {
  if (!documentationArtifactsPane) {
    return;
  }
  documentationArtifactsPane.dataset.hasArtifacts = 'true';
  documentationArtifactsPane.hidden = false;
  documentationArtifactsPane.removeAttribute('aria-hidden');
}

function updateDocumentationArtifactsFlowVisibility() {
  if (!documentationArtifactsPane) {
    return;
  }
  const hasArtifacts = documentationArtifactsPane.dataset.hasArtifacts === 'true';
  if (hasArtifacts) {
    documentationArtifactsPane.hidden = false;
    documentationArtifactsPane.removeAttribute('aria-hidden');
    return;
  }
  const flows = (documentationArtifactsPane.dataset.flows || '')
    .split(',')
    .map(value => value.trim())
    .filter(Boolean);
  const currentFlow = getSelectedFlow();
  const shouldShow = flows.length === 0 ? true : Boolean(currentFlow && flows.includes(currentFlow));
  documentationArtifactsPane.hidden = !shouldShow;
  if (documentationArtifactsPane.hidden) {
    documentationArtifactsPane.setAttribute('aria-hidden', 'true');
  } else {
    documentationArtifactsPane.removeAttribute('aria-hidden');
  }
}

function clearDocumentationArtifactsVisibility() {
  if (!documentationArtifactsPane) {
    return;
  }
  documentationArtifactsPane.dataset.hasArtifacts = 'false';
  updateDocumentationArtifactsFlowVisibility();
}

const recentProjects = [];
const recentProjectsLookup = new Map();

export function initWorkflow() {
  workflowProgressPane = document.getElementById('workflowProgressPane');
  workflowProgressSummary = document.getElementById('workflowProgressSummary');
  workflowProgressList = document.getElementById('workflowProgressList');
  workflowStacksDisplay = document.getElementById('workflowStacksDisplay');
  workflowStacksStatus = document.getElementById('workflowStacksStatus');
  workflowProjectEl = document.getElementById('workflowProject');
  workflowCollectionInput = document.getElementById('workflowCollection');
  workflowMainframeInput = document.getElementById('workflowMainframe');
  workflowGeneratorInput = document.getElementById('workflowGenerator');
  workflowGeneratorDirInput = document.getElementById('workflowGeneratorDir');
  workflowSpringInput = document.getElementById('workflowSpring');
  workflowReviewInput = document.getElementById('workflowReview');
  workflowProjectOptions = document.getElementById('workflowProjectOptions');
  workflowMainframeOptions = document.getElementById('workflowMainframeOptions');
  workflowCollectionOptions = document.getElementById('workflowCollectionOptions');
  workflowGeneratorOptions = document.getElementById('workflowGeneratorOptions');
  workflowGeneratorDirOptions = document.getElementById('workflowGeneratorDirOptions');
  workflowSpringOptions = document.getElementById('workflowSpringOptions');
  workflowStatusEl = document.getElementById('workflowStatus');
  workflowStepsEl = document.getElementById('workflowSteps');
  workflowReviewEl = document.getElementById('workflowReviewResult');
  workflowDownloadContainer = document.getElementById('workflowDownloadContainer');
  workflowDownloadBtn = document.getElementById('workflowDownload');
  workflowDownloadStatus = document.getElementById('workflowDownloadStatus');
  workflowArtifactSelect = document.getElementById('workflowArtifactSelect');
  documentationArtifactsPane = document.getElementById('documentationArtifactsPane');
  documentationArtifactsStatusEl = document.getElementById('documentationArtifactsStatus');
  documentationArtifactsListEl = document.getElementById('documentationArtifactsList');
  workflowStartBtn = document.getElementById('workflowStart');
  workflowStopBtn = document.getElementById('workflowStop');
  recentProjectsCard = document.getElementById('recentProjectsCard');
  recentProjectsSelect = document.getElementById('recentProjectsSelect');
  recentProjectInfo = document.getElementById('recentProjectInfo');
  runningWorkflowsCard = document.getElementById('runningWorkflowsCard');
  runningWorkflowsList = document.getElementById('runningWorkflowsList');
  workflowFlowGroups = Array.from(document.querySelectorAll('#workflow [data-flows]'));

  initConsolidatedDoc();
  bindProjectIdentifierInputs();
  bindFlowVisibility();
  bindWorkflowButtons();
  bindChatAndDocs();
  bindAgentRun();

  loadWorkflowDefaults();
  hydrateRecentProjects();
  fetchWorkflowStatus(false);
}

function bindProjectIdentifierInputs() {
  if (workflowProjectEl) {
    workflowProjectEl.addEventListener('input', () => {
      setProjectIdentifier(workflowProjectEl.value);
    });
    workflowProjectEl.addEventListener('change', () => {
      const projectId = setProjectIdentifier(workflowProjectEl.value);
      ensureWorkflowPolling(false);
      fetchWorkflowStatus(true, projectId);
    });
  }

  onProjectIdentifierChange(projectId => {
    if (workflowProjectEl && workflowProjectEl.value.trim() !== projectId) {
      workflowProjectEl.value = projectId;
    }
  });
}

function bindFlowVisibility() {
  const applyVisibility = flow => {
    workflowFlowGroups.forEach(group => {
      const flows = (group.dataset.flows || '')
        .split(',')
        .map(value => value.trim())
        .filter(Boolean);
      const shouldShow = flows.length === 0 ? true : Boolean(flow && flows.includes(flow));
      group.hidden = !shouldShow;
      if (group.hidden) {
        group.setAttribute('aria-hidden', 'true');
      } else {
        group.removeAttribute('aria-hidden');
      }
    });
  };

  applyVisibility(getSelectedFlow());
  onFlowChange(flow => applyVisibility(flow));
}

function bindWorkflowButtons() {
  if (workflowStartBtn) {
    workflowStartBtn.addEventListener('click', async () => {
      const projectId = getProjectIdentifier();
      if (!projectId) {
        if (workflowStatusEl) {
          workflowStatusEl.textContent = 'Project identifier is required.';
        }
        return;
      }
      setProjectIdentifier(projectId);
      if (workflowStatusEl) {
        workflowStatusEl.textContent = 'Starting workflow...';
      }
      workflowStartBtn.disabled = true;
      let shouldReenable = true;
      try {
        await startSelectedWorkflow();
        shouldReenable = false;
        if (workflowStatusEl) {
          workflowStatusEl.textContent = 'Workflow started.';
        }
        markStepComplete(5);
        ensureWorkflowPolling(true);
        fetchWorkflowStatus(true, projectId);
      } catch (err) {
        if (isWorkflowAlreadyRunningError(err)) {
          shouldReenable = false;
          if (workflowStatusEl) {
            workflowStatusEl.textContent = 'Workflow is already running.';
          }
          ensureWorkflowPolling(true);
          fetchWorkflowStatus(true, projectId);
        } else if (err && err.userFacing) {
          if (workflowStatusEl) {
            workflowStatusEl.textContent = err.message;
          }
        } else {
          const message = err && err.message ? err.message : String(err);
          if (workflowStatusEl) {
            workflowStatusEl.textContent = 'Error: ' + escapeHtml(message);
          }
        }
      } finally {
        if (shouldReenable && workflowStartBtn) {
          workflowStartBtn.disabled = false;
        }
      }
    });
  }

  if (workflowStopBtn) {
    workflowStopBtn.addEventListener('click', async () => {
      const projectId = getProjectIdentifier();
      if (!projectId) {
        if (workflowStatusEl) {
          workflowStatusEl.textContent = 'Project identifier is required to stop a workflow.';
        }
        return;
      }
      setProjectIdentifier(projectId);
      if (workflowStatusEl) {
        workflowStatusEl.textContent = 'Stopping workflow...';
      }
      workflowStopBtn.disabled = true;
      try {
        await postJSON('/v1/workflow/stop', { project_id: projectId });
        if (workflowStatusEl) {
          workflowStatusEl.textContent = 'Cancellation requested.';
        }
        ensureWorkflowPolling(true);
        fetchWorkflowStatus(true, projectId);
      } catch (err) {
        if (workflowStatusEl) {
          workflowStatusEl.textContent = 'Error: ' + escapeHtml(err.message || String(err));
        }
        ensureWorkflowPolling(false);
        fetchWorkflowStatus(true, projectId);
      }
    });
  }

  if (workflowDownloadBtn) {
    workflowDownloadBtn.addEventListener('click', async () => {
      const datasetProject = workflowDownloadBtn.dataset.projectId
        ? workflowDownloadBtn.dataset.projectId.trim()
        : '';
      const projectId = getProjectIdentifier(datasetProject);
      if (!projectId) {
        if (workflowDownloadStatus) {
          workflowDownloadStatus.textContent = 'Project identifier is required before downloading artifacts.';
        }
        return;
      }
      setProjectIdentifier(projectId);
      const selectedArtifact = workflowArtifactSelect ? workflowArtifactSelect.value.trim() : 'spring';
      if (!selectedArtifact) {
        if (workflowDownloadStatus) {
          workflowDownloadStatus.textContent = 'Select an artifact to download.';
        }
        return;
      }
      const selectedLabel = workflowArtifactSelect
        ? workflowArtifactSelect.options[workflowArtifactSelect.selectedIndex]?.textContent.trim() || ''
        : '';
      if (workflowDownloadStatus) {
        const label = selectedLabel || 'artifact';
        workflowDownloadStatus.textContent = `Preparing download for ${label}...`;
      }
      workflowDownloadBtn.disabled = true;
      if (workflowArtifactSelect) {
        workflowArtifactSelect.disabled = true;
      }
      try {
        let requestUrl = `/v1/workflow/download?project_id=${encodeURIComponent(projectId)}`;
        if (selectedArtifact && selectedArtifact !== 'spring') {
          requestUrl += `&artifact=${encodeURIComponent(selectedArtifact)}`;
        }
        const response = await fetch(requestUrl);
        if (!response.ok) {
          let message = `Download failed (${response.status})`;
          try {
            const errorPayload = await response.clone().json();
            if (errorPayload && errorPayload.error) {
              message = errorPayload.error;
            }
          } catch (jsonErr) {
            try {
              const text = await response.text();
              if (text) {
                message = text;
              }
            } catch (textErr) {
              console.warn('workflow download error parsing failed', jsonErr, textErr);
            }
          }
          throw new Error(message);
        }
        const blob = await response.blob();
        let filename = 'spring-artifact.zip';
        const disposition = response.headers.get('Content-Disposition') || '';
        const utf8Match = disposition.match(/filename\*=UTF-8''([^;]+)/i);
        if (utf8Match && utf8Match[1]) {
          try {
            filename = decodeURIComponent(utf8Match[1]);
          } catch (decodeErr) {
            console.warn('workflow download filename decode failed', decodeErr);
          }
        } else {
          const simpleMatch = disposition.match(/filename="?([^";]+)"?/i);
          if (simpleMatch && simpleMatch[1]) {
            filename = simpleMatch[1];
          }
        }
        const downloadUrl = URL.createObjectURL(blob);
        const link = document.createElement('a');
        link.href = downloadUrl;
        link.download = filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
        URL.revokeObjectURL(downloadUrl);
        if (workflowDownloadStatus) {
          const label = selectedLabel ? `${selectedLabel}: ` : '';
          workflowDownloadStatus.textContent = `Download started: ${label}${filename}`;
        }
      } catch (err) {
        const message = err && err.message ? err.message : String(err);
        if (workflowDownloadStatus) {
          workflowDownloadStatus.textContent = 'Download failed: ' + message;
        }
      } finally {
        workflowDownloadBtn.disabled = false;
        if (workflowArtifactSelect) {
          workflowArtifactSelect.disabled = false;
        }
      }
    });
  }

  if (recentProjectsSelect) {
    recentProjectsSelect.addEventListener('change', () => {
      const selectedProjectId = recentProjectsSelect.value;
      renderRecentProjectDetails(selectedProjectId);
      if (selectedProjectId) {
        fetchWorkflowStatus(true, selectedProjectId);
      }
    });
  }
}

function bindChatAndDocs() {
  const chatSendBtn = document.getElementById('chatSend');
  if (chatSendBtn) {
    chatSendBtn.addEventListener('click', async () => {
      const prompt = document.getElementById('chatPrompt').value.trim();
      const useRag = document.getElementById('chatUseRag').checked;
      const collection = document.getElementById('chatCollection').value.trim();
      const resultEl = document.getElementById('chatResult');
      resultEl.textContent = 'Sending...';
      try {
        const response = await postJSON('/v1/chat', { prompt, use_rag: useRag, collection });
        let output = `Provider: ${response.provider}\n\n${response.answer}`;
        if (response.context && response.context.length) {
          output += '\n\nContext:';
          response.context.forEach(snippet => {
            output += `\n- ${snippet}`;
          });
        }
        resultEl.textContent = output;
      } catch (err) {
        resultEl.textContent = 'Error: ' + err.message;
      }
    });
  }

  const loadProjectDocBtn = document.getElementById('loadProjectDoc');
  if (loadProjectDocBtn) {
    loadProjectDocBtn.addEventListener('click', async () => {
      await fetchProjectDoc({ showLoading: true, autoStart: true });
    });
  }

  const loadProgramDocBtn = document.getElementById('loadProgramDoc');
  if (loadProgramDocBtn) {
    loadProgramDocBtn.addEventListener('click', async () => {
      const symbol = document.getElementById('programSymbol').value.trim();
      const resultEl = document.getElementById('programDoc');
      resultEl.classList.remove('warning');
      if (!symbol) {
        resultEl.textContent = 'Enter a program symbol first.';
        return;
      }
      const projectId = getProjectIdentifier();
      if (!projectId) {
        resultEl.textContent = 'Select a project to load program documentation.';
        resultEl.classList.add('warning');
        return;
      }
      resultEl.textContent = 'Loading...';
      try {
        let url = `/api/program-doc?symbol=${encodeURIComponent(symbol)}&project_id=${encodeURIComponent(projectId)}`;
        const activeCollection = (flowConfig.knowledge.collection || '').trim();
        if (activeCollection) {
          url += `&collection=${encodeURIComponent(activeCollection)}`;
        }
        const doc = await getJSON(url);
        const toDisplayList = value => {
          if (Array.isArray(value)) {
            return value.join(', ');
          }
          return value == null ? '' : String(value);
        };
        const sections = [];
        sections.push(`<h4>${escapeHtml(doc.program || '')}</h4>`);
        if (doc.source_path) {
          sections.push(`<p><strong>Source:</strong> ${escapeHtml(doc.source_path)}</p>`);
        }
        if (doc.technologies && doc.technologies.length) {
          sections.push(`<p><strong>Technologies:</strong> ${escapeHtml(toDisplayList(doc.technologies))}</p>`);
        }
        if (doc.inputs && doc.inputs.length) {
          sections.push(`<p><strong>Inputs:</strong> ${escapeHtml(toDisplayList(doc.inputs))}</p>`);
        }
        if (doc.outputs && doc.outputs.length) {
          sections.push(`<p><strong>Outputs:</strong> ${escapeHtml(toDisplayList(doc.outputs))}</p>`);
        }
        if (doc.calls && doc.calls.length) {
          sections.push(`<p><strong>Calls:</strong> ${escapeHtml(toDisplayList(doc.calls))}</p>`);
        }
        if (doc.paragraphs && doc.paragraphs.length) {
          sections.push(`<p><strong>Paragraphs:</strong> ${escapeHtml(toDisplayList(doc.paragraphs))}</p>`);
        }
        const docBodies = [];
        if (doc.documentation_markdown) {
          docBodies.push(String(doc.documentation_markdown));
        } else if (doc.documentation) {
          if (Array.isArray(doc.documentation)) {
            docBodies.push(doc.documentation.map(item => (item == null ? '' : String(item))).join('\n\n'));
          } else {
            docBodies.push(String(doc.documentation));
          }
        }
        if (docBodies.length) {
          sections.push(`<div class="doc-markdown">${renderMarkdown(docBodies.join('\n\n'))}</div>`);
        }
        if (doc.logic && doc.logic.length) {
          const logicLines = (doc.logic || []).map(item => (item == null ? '' : String(item)));
          const logicMarkdown = ['```', ...logicLines, '```'].join('\n');
          sections.push('<h5>Logic Snippets</h5>');
          sections.push(`<div class="doc-markdown">${renderMarkdown(logicMarkdown)}</div>`);
        }
        resultEl.innerHTML = sections.join('');
        markStepComplete(3);
      } catch (err) {
        resultEl.textContent = 'Error: ' + (err.message || String(err));
      }
    });
  }
}

function bindAgentRun() {
  const agentRunBtn = document.getElementById('agentRun');
  if (!agentRunBtn) {
    return;
  }
  agentRunBtn.addEventListener('click', async () => {
    const goal = document.getElementById('agentGoal').value.trim();
    const resultEl = document.getElementById('agentResult');
    if (!goal) {
      resultEl.textContent = 'Enter a goal first.';
      return;
    }
    resultEl.textContent = 'Running agent...';
    try {
      const payload = { goal };
      const selectedFlow = getSelectedFlow();
      if (selectedFlow) {
        payload.flow = selectedFlow;
        if (knowledgeFlows.has(selectedFlow)) {
          const { repo, stacks, collection } = flowConfig.knowledge;
          if (repo || (stacks && stacks.length) || collection) {
            payload.knowledge_config = {
              repo,
              stacks: Array.isArray(stacks) ? stacks.slice() : [],
              collection
            };
          }
        }
        if (modernizationFlows.has(selectedFlow)) {
          const target = { ...flowConfig.modernization };
          const hasTargetDetails = Object.values(target).some(value => {
            if (Array.isArray(value)) {
              return value.length > 0;
            }
            return Boolean(value);
          });
          if (hasTargetDetails) {
            payload.target_config = target;
          }
        }
      }
      const response = await postJSON('/v1/agent/run', payload);
      resultEl.textContent = response.result;
    } catch (err) {
      resultEl.textContent = 'Error: ' + err.message;
    }
  });
}

export function ensureWorkflowPolling(running) {
  const projectId = getProjectIdentifier();
  if (running && projectId) {
    if (!workflowPollId) {
      workflowPollId = setInterval(() => fetchWorkflowStatus(false), 3000);
    }
  } else if (workflowPollId) {
    clearInterval(workflowPollId);
    workflowPollId = null;
  }
}

function renderWorkflowState(state, projectId) {
  updateWorkflowProgress(state);
  const requestProject = state && state.request ? (state.request.project_id || '').toString().trim() : '';
  if (requestProject) {
    setProjectIdentifier(requestProject);
  }
  const stateProjectIdRaw =
    state && state.project_id != null ? state.project_id : requestProject || projectId || getProjectIdentifier();
  const stateProjectId = stateProjectIdRaw != null ? stateProjectIdRaw.toString().trim() : '';
  const artifactPath = state && typeof state.spring_artifact === 'string' ? state.spring_artifact.trim() : '';
  const conversionArtifacts =
    state && state.conversion_artifacts && typeof state.conversion_artifacts === 'object'
      ? state.conversion_artifacts
      : {};
  const documentationArtifacts =
    state && state.documentation_artifacts && typeof state.documentation_artifacts === 'object'
      ? state.documentation_artifacts
      : {};
  const artifactOptions = [];
  if (artifactPath) {
    artifactOptions.push({ value: 'spring', label: formatArtifactLabel('spring') });
  }
  if (conversionArtifacts) {
    Object.entries(conversionArtifacts)
      .filter(([, value]) => typeof value === 'string' && value.trim())
      .sort((a, b) => a[0].localeCompare(b[0]))
      .forEach(([key]) => {
        artifactOptions.push({ value: key, label: formatArtifactLabel(key) });
      });
  }
  if (documentationArtifacts) {
    Object.entries(documentationArtifacts)
      .filter(([, value]) => typeof value === 'string' && value.trim())
      .sort((a, b) => a[0].localeCompare(b[0]))
      .forEach(([key]) => {
        artifactOptions.push({ value: key, label: formatArtifactLabel(key) });
      });
  }
  const artifactAvailable = artifactOptions.length > 0;
  if (state && stateProjectId) {
    upsertRecentProject(stateProjectId, state);
  }
  refreshRecentProjectsUI(stateProjectId);
  const activeProject = projectId || getProjectIdentifier();
  const buttonsMatch = !requestProject || !activeProject || requestProject === activeProject;
  const running = Boolean(state && state.running);
  if (workflowStartBtn) {
    workflowStartBtn.disabled = Boolean(buttonsMatch && running);
  }
  if (workflowStopBtn) {
    workflowStopBtn.disabled = !(buttonsMatch && running);
  }
  const downloadProjectId = stateProjectId || requestProject || projectId || getProjectIdentifier() || '';
  if (workflowDownloadContainer) {
    workflowDownloadContainer.hidden = !artifactAvailable;
    workflowDownloadContainer.setAttribute('aria-hidden', artifactAvailable ? 'false' : 'true');
  }
  if (workflowDownloadBtn) {
    workflowDownloadBtn.disabled = !artifactAvailable;
    workflowDownloadBtn.dataset.projectId = downloadProjectId;
  }
  if (workflowArtifactSelect) {
    const previous = workflowArtifactSelect.value;
    workflowArtifactSelect.innerHTML = '';
    artifactOptions.forEach((option, index) => {
      const optEl = document.createElement('option');
      optEl.value = option.value;
      optEl.textContent = option.label;
      if (previous === option.value || (!previous && index === 0)) {
        optEl.selected = true;
      }
      workflowArtifactSelect.appendChild(optEl);
    });
    if (!workflowArtifactSelect.value && artifactOptions.length) {
      workflowArtifactSelect.selectedIndex = 0;
    }
    workflowArtifactSelect.disabled = !artifactAvailable;
  }
  if (!artifactAvailable && workflowDownloadStatus) {
    workflowDownloadStatus.textContent = '';
  }
  if (!state || !state.status) {
    if (workflowStatusEl) {
      workflowStatusEl.textContent = 'No workflow has been started yet for this project.';
    }
    if (workflowStepsEl) {
      workflowStepsEl.textContent = '';
    }
    if (workflowReviewEl) {
      workflowReviewEl.textContent = '';
    }
    return;
  }
  if (state.started_at) {
    markStepComplete(5);
  }
  const statusLines = [];
  if (requestProject) {
    statusLines.push(`Project: ${escapeHtml(requestProject)}`);
  }
  if (state.request && state.request.flow) {
    statusLines.push(`Flow: ${escapeHtml(state.request.flow)}`);
  }
  statusLines.push(`Status: ${escapeHtml((state.status || '').toString().toUpperCase())}`);
  if (state.started_at) {
    statusLines.push(`Started: ${formatTimestamp(state.started_at)}`);
  }
  if (state.completed_at) {
    statusLines.push(`Completed: ${formatTimestamp(state.completed_at)}`);
  }
  if (state.error) {
    statusLines.push(`<span class="warning">${escapeHtml(state.error)}</span>`);
  }
  if (workflowStatusEl) {
    workflowStatusEl.innerHTML = statusLines.join('<br/>');
  }

  if (!state.steps || !state.steps.length) {
    if (workflowStepsEl) {
      workflowStepsEl.textContent = 'Awaiting workflow execution.';
    }
  } else if (workflowStepsEl) {
    workflowStepsEl.innerHTML = state.steps
      .map(step => {
        const fragments = [
          `<strong>${escapeHtml(step.name || '')}</strong>`,
          escapeHtml((step.status || '').toString())
        ];
        if (step.message) {
          fragments.push(`<div>${formatMultiline(step.message)}</div>`);
        }
        if (step.started_at) {
          fragments.push(`<div><small>Started: ${formatTimestamp(step.started_at)}</small></div>`);
        }
        if (step.completed_at) {
          fragments.push(`<div><small>Completed: ${formatTimestamp(step.completed_at)}</small></div>`);
        }
        return `<div>${fragments.join('<br/>')}</div>`;
      })
      .join('<hr/>');
  }

  if (workflowReviewEl) {
    if (state.review_answer) {
      let reviewHtml = `<h3>Review Answer</h3><p>${formatMultiline(state.review_answer)}</p>`;
      if (state.review_references && state.review_references.length) {
        reviewHtml +=
          '<h4>Top References</h4><ul>' +
          state.review_references
            .map(ref => {
              const items = [`<strong>${escapeHtml(ref.program || 'Unknown')}</strong>`];
              if (ref.score != null) {
                const score = typeof ref.score === 'number' ? ref.score.toFixed(2) : String(ref.score);
                items.push(`score: ${escapeHtml(score)}`);
              }
              const details = [];
              if (ref.source) {
                details.push(escapeHtml(ref.source));
              }
              if (ref.summary) {
                details.push(escapeHtml(ref.summary));
              }
              const detailHtml = details.length ? `<div>${details.join('<br/>')}</div>` : '';
              return `<li>${items.join(' – ')}${detailHtml}</li>`;
            })
            .join('') +
          '</ul>';
      }
      workflowReviewEl.innerHTML = reviewHtml;
    } else {
      workflowReviewEl.textContent = '';
    }
  }

  updateDocumentationArtifacts({ state, artifacts: documentationArtifacts, projectId: downloadProjectId });

  if (!running && state && state.completed_at) {
    checkConsolidatedDoc(getProjectIdentifier());
  }

  const stateFlow = state && state.request ? state.request.flow : '';
  if (pendingDocWorkflowProjectId && stateProjectId === pendingDocWorkflowProjectId && stateFlow === 'doc-generation') {
    const projectDocEl = document.getElementById('projectDoc');
    if (projectDocEl) {
      const watchingDocWorkflow = projectDocEl.dataset && projectDocEl.dataset.docWorkflowStatus === 'watching';
      if (state && state.error) {
        projectDocEl.textContent = 'Documentation workflow failed: ' + state.error;
        projectDocEl.classList.add('warning');
        delete projectDocEl.dataset.docWorkflowStatus;
        pendingDocWorkflowProjectId = null;
      } else if (!running && state && state.completed_at) {
        projectDocEl.textContent = 'Documentation workflow completed. Refreshing documentation...';
        projectDocEl.classList.remove('warning');
        delete projectDocEl.dataset.docWorkflowStatus;
        const refreshProjectId = stateProjectId || requestProject || projectId;
        pendingDocWorkflowProjectId = null;
        fetchProjectDoc({
          showLoading: false,
          autoStart: false,
          statusMessage: 'Documentation workflow completed. Refreshing documentation...',
          projectIdOverride: refreshProjectId
        }).catch(err => console.error('project doc refresh failed', err));
      } else if (watchingDocWorkflow && running) {
        projectDocEl.textContent = 'Documentation workflow is running. Check back soon for results.';
        projectDocEl.classList.remove('warning');
      }
    }
  }
}

const progressCompleteStatuses = new Set(['complete', 'completed', 'done', 'finished', 'success', 'succeeded']);
const progressCurrentStatuses = new Set(['active', 'executing', 'in_progress', 'processing', 'running', 'working']);
const progressErrorStatuses = new Set(['canceled', 'cancelled', 'error', 'failed', 'failure', 'stopped', 'aborted']);
const progressWaitingStatuses = new Set(['idle', 'waiting', 'queued', 'queue', 'queueing', 'pending', 'on_hold']);
const progressSkippedStatuses = new Set(['skipped', 'skip']);

function classifyProgressStatuses(steps) {
  const statuses = steps.map(step => {
    const normalized = (step.status || '').toString().toLowerCase();
    if (progressErrorStatuses.has(normalized)) {
      return 'error';
    }
    if (progressSkippedStatuses.has(normalized)) {
      return 'skipped';
    }
    if (progressCompleteStatuses.has(normalized) || step.completed_at) {
      return 'complete';
    }
    if (progressCurrentStatuses.has(normalized)) {
      return 'current';
    }
    if (progressWaitingStatuses.has(normalized)) {
      return 'waiting';
    }
    return 'upcoming';
  });
  let currentIndex = statuses.findIndex(status => status === 'current');
  if (currentIndex === -1) {
    currentIndex = statuses.findIndex(status => status === 'upcoming');
    if (currentIndex !== -1) {
      statuses[currentIndex] = 'current';
    }
  }
  return statuses;
}

function updateWorkflowProgress(state) {
  if (!workflowProgressPane || !workflowProgressList) {
    return;
  }
  const hasState = Boolean(
    state && (state.started_at || state.running || state.completed_at || (Array.isArray(state.steps) && state.steps.length))
  );
  workflowProgressPane.hidden = !hasState;
  if (!hasState) {
    workflowProgressList.innerHTML = '';
    if (workflowProgressSummary) {
      workflowProgressSummary.textContent = 'Workflow updates will appear here after kickoff.';
    }
    return;
  }
  if (workflowProgressSummary) {
    const summaryParts = [];
    if (state.request && state.request.project_id) {
      summaryParts.push(`Project: ${state.request.project_id}`);
    }
    if (state.request && state.request.flow) {
      summaryParts.push(`Flow: ${state.request.flow}`);
    }
    if (state.status) {
      summaryParts.push(`Status: ${state.status.toString().toUpperCase()}`);
    }
    if (state.running) {
      summaryParts.push('Workflow is currently running.');
    } else if (state.completed_at) {
      summaryParts.push('Workflow has finished.');
    }
    workflowProgressSummary.textContent = summaryParts.join(' • ') || 'Workflow status updated.';
  }
  const steps = Array.isArray(state.steps) ? state.steps : [];
  if (!steps.length) {
    workflowProgressList.innerHTML = `
      <li class="progress-step upcoming">
        <span class="progress-step-icon" aria-hidden="true"></span>
        <div class="progress-step-content">
          <span class="progress-step-name">Awaiting workflow execution</span>
          <span class="progress-step-status">Upcoming</span>
        </div>
      </li>
    `;
    return;
  }
  const statuses = classifyProgressStatuses(steps);
  workflowProgressList.innerHTML = steps
    .map((step, index) => {
      const stateClass = statuses[index] || 'upcoming';
      const statusLabel = (step.status || stateClass).toString();
      const detailLines = [];
      if (step.message) {
        detailLines.push(`<span class="progress-step-message">${formatMultiline(step.message)}</span>`);
      }
      const timeBits = [];
      if (step.started_at) {
        timeBits.push(`Started: ${formatTimestamp(step.started_at)}`);
      }
      if (step.completed_at) {
        timeBits.push(`Completed: ${formatTimestamp(step.completed_at)}`);
      }
      if (timeBits.length) {
        detailLines.push(`<span class="progress-step-message">${escapeHtml(timeBits.join(' • '))}</span>`);
      }
      return `
        <li class="progress-step ${stateClass}">
          <span class="progress-step-icon" aria-hidden="true"></span>
          <div class="progress-step-content">
            <span class="progress-step-name">${escapeHtml(step.name || `Step ${index + 1}`)}</span>
            <span class="progress-step-status">${escapeHtml(statusLabel.toUpperCase())}</span>
            ${detailLines.join('')}
          </div>
        </li>
      `;
    })
    .join('');
}

function updateDocumentationArtifacts({ state, artifacts, projectId }) {
  if (!documentationArtifactsStatusEl || !documentationArtifactsListEl) {
    return;
  }
  if (documentationArtifactsPane && !documentationArtifactsPane.dataset.hasArtifacts) {
    documentationArtifactsPane.dataset.hasArtifacts = 'false';
  }
  const normalizedProject = projectId == null ? '' : String(projectId).trim();
  const signature = buildDocumentationArtifactSignature(artifacts);
  const flow = state && state.request ? state.request.flow : '';
  const running = Boolean(state && state.running);
  const isDocWorkflow = flow === 'doc-generation';
  const isRunningDocWorkflow = running && isDocWorkflow;
  const projectChanged = documentationArtifactsProjectId !== normalizedProject;

  if (projectChanged) {
    documentationArtifactsProjectId = normalizedProject;
    documentationArtifactsSignature = '';
    if (documentationArtifactsFetchController) {
      documentationArtifactsFetchController.abort();
      documentationArtifactsFetchController = null;
    }
    if (!normalizedProject) {
      clearDocumentationArtifactsVisibility();
      resetDocumentationArtifacts('Documentation artifacts will appear here after a workflow completes.');
      return;
    }
  }

  if (isRunningDocWorkflow && !documentationArtifactsRunning) {
    documentationArtifactsSignature = '';
    clearDocumentationArtifactsVisibility();
    resetDocumentationArtifacts('Documentation workflow is running. Artifacts will refresh when it completes.');
  }

  if (!isRunningDocWorkflow && documentationArtifactsRunning) {
    documentationArtifactsSignature = '';
    if (signature) {
      setDocumentationStatus('Documentation workflow completed. Refreshing artifacts…');
    } else {
      resetDocumentationArtifacts('Documentation workflow completed. Waiting for artifacts to publish.');
    }
  }

  documentationArtifactsRunning = isRunningDocWorkflow;

  if (!normalizedProject) {
    clearDocumentationArtifactsVisibility();
    return;
  }

  if (!signature) {
    if (isRunningDocWorkflow) {
      setDocumentationStatus('Documentation workflow is running. Artifacts will refresh when it completes.');
    } else {
      resetDocumentationArtifacts('No documentation artifacts are available for this project yet.');
    }
    clearDocumentationArtifactsVisibility();
    return;
  }

  ensureDocumentationArtifactsVisible();

  if (signature === documentationArtifactsSignature && !projectChanged) {
    return;
  }

  fetchDocumentationArtifacts(normalizedProject, signature);
}

function buildDocumentationArtifactSignature(artifacts) {
  if (!artifacts || typeof artifacts !== 'object') {
    return '';
  }
  const entries = Object.entries(artifacts)
    .filter(([, value]) => typeof value === 'string' && value.trim())
    .map(([key, value]) => `${key}:${value.trim()}`);
  if (!entries.length) {
    return '';
  }
  entries.sort((a, b) => a.localeCompare(b));
  return entries.join('|');
}

function setDocumentationStatus(message, warning = false) {
  if (!documentationArtifactsStatusEl) {
    return;
  }
  documentationArtifactsStatusEl.textContent = message || '';
  if (warning) {
    documentationArtifactsStatusEl.classList.add('warning');
  } else {
    documentationArtifactsStatusEl.classList.remove('warning');
  }
}

function resetDocumentationArtifacts(message, warning = false) {
  if (documentationArtifactsListEl) {
    documentationArtifactsListEl.innerHTML = '';
  }
  setDocumentationStatus(message, warning);
}

async function fetchDocumentationArtifacts(projectId, signature) {
  if (!projectId) {
    return;
  }
  if (documentationArtifactsFetchController) {
    documentationArtifactsFetchController.abort();
  }
  const controller = new AbortController();
  documentationArtifactsFetchController = controller;
  if (documentationArtifactsListEl) {
    documentationArtifactsListEl.innerHTML = '';
  }
  setDocumentationStatus('Loading documentation artifacts…');
  try {
    const response = await fetch(`/v1/workflow/documentation?project_id=${encodeURIComponent(projectId)}`, {
      signal: controller.signal
    });
    let payload;
    try {
      payload = await response.json();
    } catch (parseErr) {
      payload = { error: 'Invalid JSON response' };
    }
    if (!response.ok) {
      const message = payload && payload.error ? payload.error : response.statusText || 'Request failed';
      const error = new Error(message);
      error.status = response.status;
      throw error;
    }
    if (controller.signal.aborted) {
      return;
    }
    renderDocumentationArtifacts(payload ? payload.artifacts : null);
    documentationArtifactsSignature = signature;
  } catch (err) {
    if (controller.signal.aborted) {
      return;
    }
    const message = err && err.message ? err.message : String(err);
    setDocumentationStatus(`Failed to load documentation artifacts: ${message}`, true);
    documentationArtifactsSignature = '';
  } finally {
    if (documentationArtifactsFetchController === controller) {
      documentationArtifactsFetchController = null;
    }
  }
}

function renderDocumentationArtifacts(artifactMap) {
  if (!documentationArtifactsListEl) {
    return;
  }
  documentationArtifactsListEl.innerHTML = '';
  const entries = artifactMap && typeof artifactMap === 'object' ? Object.entries(artifactMap) : [];
  const typeFragments = entries
    .filter(([, programs]) => Array.isArray(programs) && programs.length)
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([type, programs]) => {
      const programFragments = programs
        .map(program => renderDocumentationProgram(type, program))
        .filter(Boolean);
      if (!programFragments.length) {
        return '';
      }
      const typeLabel = formatArtifactLabel(type);
      const countLabel = programFragments.length === 1 ? '1 program' : `${programFragments.length} programs`;
      return `
        <article class="documentation-type">
          <div class="documentation-type-header">
            <h4 class="documentation-type-title">${escapeHtml(typeLabel)}</h4>
            <span class="documentation-program-count">${escapeHtml(countLabel)}</span>
          </div>
          ${programFragments.join('')}
        </article>
      `;
    })
    .filter(Boolean);

  if (!typeFragments.length) {
    resetDocumentationArtifacts('No documentation artifacts are available for this project yet.');
    clearDocumentationArtifactsVisibility();
    return;
  }

  documentationArtifactsListEl.innerHTML = typeFragments.join('');
  setDocumentationStatus('Documentation artifacts updated.');
  ensureDocumentationArtifactsVisible();
  maybeRenderMermaid(documentationArtifactsListEl);
}

function renderDocumentationProgram(typeKey, programGroup) {
  if (!programGroup || !Array.isArray(programGroup.documents)) {
    return '';
  }
  const documents = programGroup.documents
    .map(doc => renderDocumentationDocument(typeKey, doc))
    .filter(Boolean);
  if (!documents.length) {
    return '';
  }
  const programName = programGroup.program == null ? '' : String(programGroup.program).trim();
  const label = programName || 'Project';
  const countLabel = documents.length === 1 ? '1 artifact' : `${documents.length} artifacts`;
  return `
    <section class="documentation-program">
      <div class="documentation-program-header">
        <h5 class="documentation-program-title">${escapeHtml(label)}</h5>
        <span class="documentation-program-count">${escapeHtml(countLabel)}</span>
      </div>
      ${documents.join('')}
    </section>
  `;
}

function renderDocumentationDocument(typeKey, doc) {
  if (!doc || doc.markdown == null) {
    return '';
  }
  const markdown = String(doc.markdown).trim();
  if (!markdown) {
    return '';
  }
  const summary = doc.summary == null ? '' : String(doc.summary).trim();
  const title = summary || formatArtifactLabel(doc.type || typeKey);
  const sourcePath = doc.source_path == null ? '' : String(doc.source_path).trim();
  const sections = ['<div class="documentation-doc">'];
  if (title) {
    sections.push(`<h6 class="documentation-doc-title">${escapeHtml(title)}</h6>`);
  }
  if (sourcePath) {
    sections.push(`<div class="documentation-program-source">Source: ${escapeHtml(sourcePath)}</div>`);
  }
  sections.push(`<div class="doc-markdown">${renderMarkdown(markdown)}</div>`);
  sections.push('</div>');
  return sections.join('');
}

function prettifyFieldName(name) {
  return name
    .split('_')
    .filter(Boolean)
    .map(part => part.charAt(0).toUpperCase() + part.slice(1))
    .join(' ');
}

function renderRecentProjectDetails(projectId) {
  if (!recentProjectInfo) {
    return;
  }
  if (!projectId || !recentProjectsLookup.has(projectId)) {
    recentProjectInfo.textContent = recentProjects.length ? 'Select a project to view details.' : '';
    return;
  }
  const project = recentProjectsLookup.get(projectId);
  const detailParts = [];
  detailParts.push(`<div><strong>Project:</strong> ${escapeHtml(project.id)}</div>`);
  if (project.flow) {
    detailParts.push(`<div><strong>Flow:</strong> ${escapeHtml(project.flow)}</div>`);
  }
  if (typeof project.documents === 'number' && Number.isFinite(project.documents)) {
    detailParts.push(`<div><strong>Documents:</strong> ${project.documents}</div>`);
  }
  if (project.status) {
    detailParts.push(`<div><strong>Status:</strong> ${escapeHtml(project.status.toString().toUpperCase())}</div>`);
  }
  if (project.started_at) {
    const startedTs = formatTimestamp(project.started_at);
    if (startedTs) {
      detailParts.push(`<div><strong>Started:</strong> ${startedTs}</div>`);
    }
  }
  if (project.completed_at) {
    const completedTs = formatTimestamp(project.completed_at);
    if (completedTs) {
      detailParts.push(`<div><strong>Completed:</strong> ${completedTs}</div>`);
    }
  }
  if (project.running != null) {
    detailParts.push(`<div><strong>Running:</strong> ${project.running ? 'Yes' : 'No'}</div>`);
  }
  if (project.steps && project.steps.length) {
    const latestStep = project.steps[project.steps.length - 1];
    const stepBits = [];
    if (latestStep.name) {
      stepBits.push(escapeHtml(latestStep.name));
    }
    if (latestStep.status) {
      stepBits.push(escapeHtml(latestStep.status.toString()));
    }
    if (latestStep.message) {
      stepBits.push(escapeHtml(latestStep.message));
    }
    if (stepBits.length) {
      detailParts.push(`<div><strong>Latest Step:</strong> ${stepBits.join(' – ')}</div>`);
    }
  }
  const request = project.request || {};
  const requestDetails = [];
  Object.entries(request).forEach(([key, value]) => {
    if (key === 'project_id' || key === 'flow' || value == null || value === '') {
      return;
    }
    let displayValue = '';
    if (Array.isArray(value)) {
      displayValue = value.join(', ');
    } else if (typeof value === 'object') {
      try {
        displayValue = JSON.stringify(value);
      } catch (err) {
        displayValue = String(value);
      }
    } else {
      displayValue = String(value);
    }
    if (!displayValue) {
      return;
    }
    requestDetails.push(`<div><strong>${escapeHtml(prettifyFieldName(key))}:</strong> ${escapeHtml(displayValue)}</div>`);
  });
  if (requestDetails.length) {
    detailParts.push('<hr/>');
    detailParts.push('<div><strong>Request Details</strong></div>');
    detailParts.push(...requestDetails);
  }
  recentProjectInfo.innerHTML = detailParts.join('');
}

function refreshRecentProjectsUI(activeProjectId) {
  if (!recentProjectsCard || !recentProjectsSelect || !recentProjectInfo) {
    return;
  }
  if (!recentProjects.length) {
    recentProjectsCard.hidden = true;
    recentProjectsSelect.innerHTML = '';
    recentProjectsSelect.disabled = true;
    recentProjectInfo.textContent = '';
    if (runningWorkflowsCard && runningWorkflowsList) {
      runningWorkflowsCard.hidden = true;
      runningWorkflowsList.innerHTML = '';
    }
    return;
  }
  const previousSelection = recentProjectsSelect.value;
  recentProjectsCard.hidden = false;
  recentProjectsSelect.disabled = false;
  recentProjectsSelect.innerHTML = '';
  const fragment = document.createDocumentFragment();
  recentProjects.forEach(project => {
    const option = document.createElement('option');
    option.value = project.id;
    option.textContent = project.id;
    fragment.appendChild(option);
  });
  recentProjectsSelect.appendChild(fragment);
  let selection = '';
  if (activeProjectId && recentProjectsLookup.has(activeProjectId)) {
    selection = activeProjectId;
  } else if (previousSelection && recentProjectsLookup.has(previousSelection)) {
    selection = previousSelection;
  } else {
    selection = recentProjects[0].id;
  }
  recentProjectsSelect.value = selection;
  renderRecentProjectDetails(selection);
  renderRunningWorkflows(selection);
}

function upsertRecentProject(projectId, state, documents) {
  if (!projectId) {
    return;
  }
  const existingIndex = recentProjects.findIndex(project => project.id === projectId);
  let previousDocuments = null;
  if (existingIndex !== -1) {
    if (typeof recentProjects[existingIndex].documents === 'number') {
      previousDocuments = recentProjects[existingIndex].documents;
    }
    recentProjects.splice(existingIndex, 1);
  }
  if (previousDocuments == null && recentProjectsLookup.has(projectId)) {
    const existing = recentProjectsLookup.get(projectId);
    if (existing && typeof existing.documents === 'number') {
      previousDocuments = existing.documents;
    }
  }
  let documentCount = null;
  if (typeof documents === 'number' && Number.isFinite(documents)) {
    documentCount = documents;
  } else if (state && typeof state.documents === 'number' && Number.isFinite(state.documents)) {
    documentCount = state.documents;
  }
  const requestClone = state && state.request ? { ...state.request } : {};
  const flowValue = requestClone && requestClone.flow ? requestClone.flow : state && state.flow ? state.flow : '';
  const runningValue = state && typeof state.running === 'boolean' ? state.running : null;
  const projectData = {
    id: projectId,
    flow: flowValue,
    status: state && state.status != null ? state.status : '',
    started_at: state && state.started_at ? state.started_at : '',
    completed_at: state && state.completed_at ? state.completed_at : '',
    running: runningValue,
    request: requestClone,
    steps: Array.isArray(state && state.steps) ? state.steps.map(step => ({ ...step })) : [],
    documents: documentCount != null ? documentCount : previousDocuments,
    updated_at: Date.now()
  };
  recentProjects.unshift(projectData);
  recentProjectsLookup.set(projectId, projectData);
  while (recentProjects.length > 5) {
    const removed = recentProjects.pop();
    if (removed) {
      recentProjectsLookup.delete(removed.id);
    }
  }
}

function renderRunningWorkflows(activeProjectId) {
  if (!runningWorkflowsCard || !runningWorkflowsList) {
    return;
  }
  const runningProjects = recentProjects.filter(project => project.running === true);
  if (!runningProjects.length) {
    runningWorkflowsCard.hidden = true;
    runningWorkflowsList.innerHTML = '';
    return;
  }
  runningWorkflowsCard.hidden = false;
  runningWorkflowsList.innerHTML = '';
  const fragment = document.createDocumentFragment();
  runningProjects.forEach(project => {
    const item = document.createElement('li');
    item.className = 'running-workflow-item' + (project.id === activeProjectId ? ' active' : '');
    item.dataset.projectId = project.id;
    item.tabIndex = 0;
    item.setAttribute('role', 'button');
    item.setAttribute('aria-pressed', project.id === activeProjectId ? 'true' : 'false');
    item.setAttribute('aria-label', `View workflow ${project.id}`);

    const header = document.createElement('div');
    header.className = 'running-workflow-header';

    const idEl = document.createElement('span');
    idEl.className = 'running-workflow-id';
    idEl.textContent = project.id;

    const lastStep = Array.isArray(project.steps) && project.steps.length ? project.steps[project.steps.length - 1] : null;
    const statusSource = project.status || (lastStep && lastStep.status) || 'Running';
    const statusEl = document.createElement('span');
    statusEl.className = 'running-workflow-status';
    statusEl.textContent = statusSource.toString().toUpperCase();

    header.appendChild(idEl);
    header.appendChild(statusEl);

    const summaryEl = document.createElement('span');
    summaryEl.className = 'running-workflow-summary';
    let summaryText = '';
    if (lastStep) {
      summaryText = lastStep.message || lastStep.name || '';
    }
    if (!summaryText) {
      summaryText = `Last update: ${statusSource}`;
    }
    summaryEl.textContent = summaryText.toString();

    item.appendChild(header);
    item.appendChild(summaryEl);

    const triggerStatusFetch = () => {
      fetchWorkflowStatus(true, project.id);
    };
    item.addEventListener('click', triggerStatusFetch);
    item.addEventListener('keydown', event => {
      if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault();
        triggerStatusFetch();
      }
    });

    fragment.appendChild(item);
  });
  runningWorkflowsList.appendChild(fragment);
}

export async function fetchWorkflowStatus(manual, overrideProjectId) {
  const overrideValue = typeof overrideProjectId === 'string' ? overrideProjectId : '';
  const hasOverride = Boolean(overrideValue && overrideValue.trim());
  const projectId = getProjectIdentifier(overrideValue);
  if (!projectId) {
    if (workflowStartBtn) {
      workflowStartBtn.disabled = false;
    }
    if (workflowStopBtn) {
      workflowStopBtn.disabled = true;
    }
    if (!hasOverride) {
      if (workflowStatusEl) {
        workflowStatusEl.textContent = 'Enter a project identifier to monitor workflow status.';
      }
      if (workflowStepsEl) {
        workflowStepsEl.textContent = '';
      }
      if (workflowReviewEl) {
        workflowReviewEl.textContent = '';
      }
      updateWorkflowProgress(null);
    }
    ensureWorkflowPolling(false);
    return;
  }
  setProjectIdentifier(projectId);
  try {
    const state = await getJSON(`/v1/workflow/status?project_id=${encodeURIComponent(projectId)}`);
    renderWorkflowState(state, projectId);
    ensureWorkflowPolling(Boolean(state && state.running));
  } catch (err) {
    const message = err && err.message ? err.message : String(err);
    if (workflowStatusEl) {
      workflowStatusEl.textContent = `Error fetching status for ${projectId}: ${message}`;
    }
    if (workflowStepsEl) {
      workflowStepsEl.textContent = '';
    }
    if (workflowReviewEl) {
      workflowReviewEl.textContent = '';
    }
    updateWorkflowProgress(null);
    ensureWorkflowPolling(false);
  }
}

export function createUserFacingError(message) {
  const error = new Error(message);
  error.userFacing = true;
  return error;
}

export function isWorkflowAlreadyRunningError(err) {
  if (!err) {
    return false;
  }
  const message = typeof err.message === 'string' && err.message ? err.message : String(err || '');
  return /workflow already running/i.test(message);
}

function parseMappingText(rawText) {
  if (typeof rawText !== 'string' || !rawText.trim()) {
    return {};
  }
  const mapping = {};
  rawText
    .replace(/\r\n/g, '\n')
    .split('\n')
    .forEach(line => {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#')) {
        return;
      }
      const separators = ['=>', '->', ':', '='];
      let separator = '';
      let index = -1;
      for (const candidate of separators) {
        const candidateIndex = trimmed.indexOf(candidate);
        if (candidateIndex !== -1) {
          separator = candidate;
          index = candidateIndex;
          break;
        }
      }
      if (index === -1) {
        return;
      }
      const from = trimmed.slice(0, index).trim();
      const to = trimmed.slice(index + separator.length).trim();
      if (!from || !to) {
        return;
      }
      mapping[from] = to;
    });
  return mapping;
}

function buildMigrationConfigPayload(state) {
  if (!state) {
    return null;
  }
  const source = state.source || {};
  const target = state.target || {};
  const mappingText = state.mappingText || {};
  const angularSourceRoot = typeof source.sourceRoot === 'string' ? source.sourceRoot.trim() : '';
  const angularVersion = typeof source.version === 'string' ? source.version.trim() : '';
  const reactTargetRoot = typeof target.targetRoot === 'string' ? target.targetRoot.trim() : '';
  const reactVersion = typeof target.version === 'string' ? target.version.trim() : '';

  const componentLifecycle = parseMappingText(mappingText.componentLifecycle);
  const directiveConversions = parseMappingText(mappingText.directiveConversions);
  const serviceContexts = parseMappingText(mappingText.serviceContexts);
  const pipeConversions = parseMappingText(mappingText.pipeConversions);
  const guardRoutes = parseMappingText(mappingText.guardRoutes);

  const hasMappings =
    Object.keys(componentLifecycle).length ||
    Object.keys(directiveConversions).length ||
    Object.keys(serviceContexts).length ||
    Object.keys(pipeConversions).length ||
    Object.keys(guardRoutes).length;
  const hasRequired = angularSourceRoot && angularVersion && reactTargetRoot && reactVersion;

  if (!hasRequired && !hasMappings) {
    return null;
  }
  if (!hasRequired) {
    return null;
  }

  const payload = {
    angular_source_root: angularSourceRoot,
    angular_version: angularVersion,
    react_target_root: reactTargetRoot,
    react_version: reactVersion
  };

  if (Object.keys(componentLifecycle).length) {
    payload.component_lifecycle_mapping = componentLifecycle;
  }
  if (Object.keys(directiveConversions).length) {
    payload.directive_conversion_mapping = directiveConversions;
  }
  if (Object.keys(serviceContexts).length) {
    payload.service_context_mapping = serviceContexts;
  }
  if (Object.keys(pipeConversions).length) {
    payload.pipe_conversion_mapping = pipeConversions;
  }
  if (Object.keys(guardRoutes).length) {
    payload.guard_route_mapping = guardRoutes;
  }

  return payload;
}

export function buildWorkflowStartPayload(options = {}) {
  const overrides = options && typeof options.overrides === 'object' ? options.overrides : null;
  const projectId = getProjectIdentifier();
  if (!projectId) {
    throw createUserFacingError('Project identifier is required.');
  }
  setProjectIdentifier(projectId);

  let mainframe = workflowMainframeInput ? workflowMainframeInput.value.trim() : '';
  let collection = workflowCollectionInput ? workflowCollectionInput.value.trim() : '';

  const selectedFlow = getSelectedFlow();
  const stacks = flowConfig.knowledge.stacks.slice();

  if (knowledgeFlows.has(selectedFlow)) {
    const trimmedKnowledgeRepo = (flowConfig.knowledge.repo || '').trim();
    if (!mainframe && trimmedKnowledgeRepo) {
      mainframe = flowConfig.knowledge.repo;
      if (workflowMainframeInput) {
        workflowMainframeInput.value = mainframe;
      }
    }
    if (!collection && flowConfig.knowledge.collection) {
      collection = flowConfig.knowledge.collection;
      if (workflowCollectionInput) {
        workflowCollectionInput.value = collection;
      }
    }
  }

  if (!mainframe) {
    throw createUserFacingError('Mainframe repository path is required.');
  }

  const generator = workflowGeneratorInput ? workflowGeneratorInput.value.trim() : '';
  const generatorDir = workflowGeneratorDirInput ? workflowGeneratorDirInput.value.trim() : '';
  const spring = workflowSpringInput ? workflowSpringInput.value.trim() : '';
  const review = workflowReviewInput ? workflowReviewInput.value.trim() : '';
  const reset = document.getElementById('workflowReset')?.checked || false;
  const verify = document.getElementById('workflowVerify')?.checked || false;

  const payload = {
    project_id: projectId,
    mainframe,
    stacks,
    collection,
    generator,
    generator_dir: generatorDir,
    spring,
    review,
    reset_store: reset,
    verify_spring: verify,
    flow: selectedFlow
  };

  if (knowledgeFlows.has(selectedFlow)) {
    payload.knowledge_config = {
      repo: flowConfig.knowledge.repo,
      stacks: flowConfig.knowledge.stacks.slice(),
      collection: flowConfig.knowledge.collection
    };
  }

  const targetPreset = getTargetPresetConfig(selectedFlow);
  if (targetPreset) {
    payload.target_config = targetPreset;
  }

  if (angularMigrationFlows.has(selectedFlow)) {
    const migrationPayload = buildMigrationConfigPayload(migrationConfigState);
    if (migrationPayload) {
      payload.migration_config = migrationPayload;
    }
  }

  if (overrides) {
    Object.assign(payload, overrides);
  }

  return { payload, projectId };
}

export async function startSelectedWorkflow(options = {}) {
  const { payload } = buildWorkflowStartPayload(options);
  return postJSON('/v1/workflow/start', payload);
}

export async function fetchProjectDoc({ showLoading = true, autoStart = false, statusMessage = '', projectIdOverride = '' } = {}) {
  const resultEl = document.getElementById('projectDoc');
  if (!resultEl) {
    return;
  }

  resultEl.classList.remove('warning');

  const projectId = getProjectIdentifier(projectIdOverride);
  if (!projectId) {
    resultEl.textContent = 'Select a project to load the project overview.';
    resultEl.classList.add('warning');
    pendingDocWorkflowProjectId = null;
    delete resultEl.dataset.docWorkflowStatus;
    return;
  }

  if (statusMessage) {
    resultEl.textContent = statusMessage;
  } else if (showLoading) {
    resultEl.textContent = 'Loading...';
  }

  const selectedFlow = getSelectedFlow();
  const shouldAutoStartWorkflow = autoStart && selectedFlow === 'doc-generation';
  let watchingWorkflow = false;

  if (shouldAutoStartWorkflow) {
    pendingDocWorkflowProjectId = projectId;
    resultEl.dataset.docWorkflowStatus = 'watching';
    try {
      await startSelectedWorkflow();
      watchingWorkflow = true;
      markStepComplete(5);
      resultEl.textContent = 'Documentation workflow started automatically. Loading latest overview...';
      ensureWorkflowPolling(true);
      fetchWorkflowStatus(true, projectId);
    } catch (err) {
      if (isWorkflowAlreadyRunningError(err)) {
        watchingWorkflow = true;
        resultEl.textContent = 'Documentation workflow is already running. Loading latest overview...';
        ensureWorkflowPolling(true);
        fetchWorkflowStatus(true, projectId);
      } else if (err && err.userFacing) {
        resultEl.textContent = err.message;
        resultEl.classList.add('warning');
        delete resultEl.dataset.docWorkflowStatus;
        pendingDocWorkflowProjectId = null;
        return;
      } else {
        const message = err && err.message ? err.message : String(err);
        resultEl.textContent = 'Error starting documentation workflow: ' + escapeHtml(message);
        resultEl.classList.add('warning');
        delete resultEl.dataset.docWorkflowStatus;
        pendingDocWorkflowProjectId = null;
        return;
      }
    }
  } else if (!statusMessage) {
    delete resultEl.dataset.docWorkflowStatus;
    pendingDocWorkflowProjectId = null;
  }

  try {
    let url = `/api/project-doc?project_id=${encodeURIComponent(projectId)}`;
    const activeCollection = (flowConfig.knowledge.collection || '').trim();
    if (activeCollection) {
      url += `&collection=${encodeURIComponent(activeCollection)}`;
    }
    const doc = await getJSON(url);
    if (!doc.programs || !doc.programs.length) {
      if (watchingWorkflow || shouldAutoStartWorkflow) {
        resultEl.textContent = 'Documentation workflow is running. Check back soon for results.';
      } else {
        resultEl.textContent = 'No programs indexed yet.';
      }
      return;
    }

    delete resultEl.dataset.docWorkflowStatus;

    let html =
      '<table><thead><tr>' +
      '<th>Program</th>' +
      '<th>Technologies</th>' +
      '<th>Inputs</th>' +
      '<th>Outputs</th>' +
      '<th>Calls</th>' +
      '<th>Summaries</th>' +
      '<th>Cross-References</th>' +
      '<th>Impact Assessments</th>' +
      '<th>Flows</th>' +
      '<th>Working Notes</th>' +
      '<th>Business Rules</th>' +
      '<th>Modernization</th>' +
      '<th>Chunks</th>' +
      '</tr></thead><tbody>';

    doc.programs.forEach(program => {
      const technologies = (program.technologies || []).map(escapeHtml).join(', ');
      const inputs = (program.inputs || []).map(escapeHtml).join(', ');
      const outputs = (program.outputs || []).map(escapeHtml).join(', ');
      const calls = (program.calls || []).map(escapeHtml).join(', ');
      const summaries = program.documentation_summaries || [];
      const crossRefs = program.cross_reference_maps || [];
      const impactAssessments = program.impact_assessments || [];
      const flows = program.flows || [];
      const workingNotes = program.working_notes || [];
      const businessRules = program.business_rules || [];
      const modernization = program.modernization || [];
      const chunks = program.chunks || [];

      html +=
        '<tr>' +
        `<td>${escapeHtml(program.program || '')}</td>` +
        `<td>${technologies}</td>` +
        `<td>${inputs}</td>` +
        `<td>${outputs}</td>` +
        `<td>${calls}</td>` +
        `<td>${summaries.length}</td>` +
        `<td>${crossRefs.length}</td>` +
        `<td>${impactAssessments.length}</td>` +
        `<td>${flows.length}</td>` +
        `<td>${workingNotes.length}</td>` +
        `<td>${businessRules.length}</td>` +
        `<td>${modernization.length}</td>` +
        `<td>${chunks.length}</td>` +
        '</tr>';
    });
    html += '</tbody></table>';

    const formatDocFamily = (label, docs) => {
      if (!docs || !docs.length) {
        return '';
      }
      const maxItems = 3;
      const items = docs.slice(0, maxItems).map(docItem => {
        const headerParts = [];
        if (docItem.source_path) {
          headerParts.push(docItem.source_path);
        }
        if (typeof docItem.chunk === 'number' && docItem.chunk >= 0) {
          headerParts.push(`chunk ${docItem.chunk}`);
        }
        const header = headerParts.length ? headerParts.join(' • ') : docItem.id || label;
        const fallback = docItem.summary || docItem.content || '';
        let bodyHtml = '';
        if (fallback) {
          bodyHtml += `<div class="doc-markdown">${renderMarkdown(fallback)}</div>`;
        }
        if (label === 'Flows' && docItem.flow_diagram) {
          const diagramHtml = renderFlowDiagramBlock(docItem.flow_diagram);
          if (diagramHtml) {
            bodyHtml += diagramHtml;
          }
        }
        return `<li><strong>${escapeHtml(header)}</strong>${bodyHtml}</li>`;
      });
      if (docs.length > maxItems) {
        const remaining = docs.length - maxItems;
        items.push('<li>' + escapeHtml(`...and ${remaining} more`) + '</li>');
      }
      return `<h5>${escapeHtml(label)}</h5><ul>${items.join('')}</ul>`;
    };

    const details = doc.programs
      .map(program => {
        const sections = [];
        sections.push(`<h4>${escapeHtml(program.program || '')}</h4>`);
        const families = [
          ['Summaries', program.documentation_summaries],
          ['Cross-References', program.cross_reference_maps],
          ['Impact Assessments', program.impact_assessments],
          ['Flows', program.flows],
          ['Working Notes', program.working_notes],
          ['Business Rules', program.business_rules],
          ['Modernization', program.modernization],
          ['Chunks', program.chunks]
        ];
        families.forEach(([label, docs]) => {
          const section = formatDocFamily(label, docs);
          if (section) {
            sections.push(section);
          }
        });
        return `<div class="project-doc-family">${sections.join('')}</div>`;
      })
      .join('');

    resultEl.innerHTML = html + details;
    maybeRenderMermaid(resultEl);
    markStepComplete(3);
  } catch (err) {
    const message = err && err.message ? err.message : String(err);
    resultEl.textContent = 'Error: ' + escapeHtml(message);
    resultEl.classList.add('warning');
  }
}

async function loadWorkflowDefaults() {
  try {
    const defaults = await getJSON('/v1/workflow/defaults');
    if (!defaults || typeof defaults !== 'object') {
      return;
    }

    const projectIds = Array.isArray(defaults.project_ids) ? defaults.project_ids : [];
    const mainframeRoots = Array.isArray(defaults.mainframe_roots) ? defaults.mainframe_roots : [];
    const collections = Array.isArray(defaults.collections) ? defaults.collections : [];
    const generatorCommands = Array.isArray(defaults.generator_commands) ? defaults.generator_commands : [];
    const generatorDirs = Array.isArray(defaults.generator_dirs) ? defaults.generator_dirs : [];
    const springProjects = Array.isArray(defaults.spring_projects) ? defaults.spring_projects : [];
    const reviewPrompts = Array.isArray(defaults.review_prompts) ? defaults.review_prompts : [];

    populateDatalist(workflowProjectOptions, projectIds);
    populateDatalist(workflowMainframeOptions, mainframeRoots);
    populateDatalist(workflowCollectionOptions, collections);
    populateDatalist(workflowGeneratorOptions, generatorCommands);
    populateDatalist(workflowGeneratorDirOptions, generatorDirs);
    populateDatalist(workflowSpringOptions, springProjects);

    if (workflowProjectEl && (!workflowProjectEl.value || !workflowProjectEl.value.trim()) && projectIds.length) {
      setProjectIdentifier(projectIds[0]);
    }

    let repoSeeded = false;
    if (workflowMainframeInput && (!workflowMainframeInput.value || !workflowMainframeInput.value.trim()) && mainframeRoots.length) {
      const repoPath = mainframeRoots[0];
      workflowMainframeInput.value = repoPath;
      repoSeeded = true;
      if (!flowConfig.knowledge.repo) {
        flowConfig.knowledge.repo = repoPath;
      }
    }

    if (workflowCollectionInput && (!workflowCollectionInput.value || !workflowCollectionInput.value.trim()) && collections.length) {
      const collectionValue = collections[0];
      workflowCollectionInput.value = collectionValue;
      if (!flowConfig.knowledge.collection) {
        flowConfig.knowledge.collection = collectionValue;
      }
    }

    if (workflowGeneratorInput && (!workflowGeneratorInput.value || !workflowGeneratorInput.value.trim()) && generatorCommands.length) {
      workflowGeneratorInput.value = generatorCommands[0];
    }

    if (workflowGeneratorDirInput && (!workflowGeneratorDirInput.value || !workflowGeneratorDirInput.value.trim()) && generatorDirs.length) {
      workflowGeneratorDirInput.value = generatorDirs[0];
    }

    if (workflowSpringInput && (!workflowSpringInput.value || !workflowSpringInput.value.trim()) && springProjects.length) {
      workflowSpringInput.value = springProjects[0];
    }

    if (workflowReviewInput && (!workflowReviewInput.value || !workflowReviewInput.value.trim()) && reviewPrompts.length) {
      workflowReviewInput.value = reviewPrompts[0];
    }

    if (repoSeeded) {
      const detectEvent = new CustomEvent('workflowRepoSeeded', { detail: { repo: workflowMainframeInput.value } });
      document.dispatchEvent(detectEvent);
    }
  } catch (err) {
    console.warn('Failed to load workflow defaults', err);
  }
}

async function hydrateRecentProjects() {
  try {
    const data = await getJSON('/v1/projects');
    const projects = Array.isArray(data.projects) ? data.projects : [];
    const existingProjectId = getProjectIdentifier() || '';
    let firstRunningProjectId = '';
    projects.forEach(item => {
      if (!item) {
        return;
      }
      const id = (item.project_id || '').toString().trim();
      if (!id) {
        return;
      }
      const state = item.state ? { ...item.state } : { request: { project_id: id } };
      if (!firstRunningProjectId && state && state.running === true) {
        firstRunningProjectId = id;
      }
      const documents = typeof item.documents === 'number' ? item.documents : undefined;
      upsertRecentProject(id, state, documents);
    });
    const preferredProjectId = existingProjectId || firstRunningProjectId || '';
    refreshRecentProjectsUI(preferredProjectId);
    const defaultProjectSelection =
      recentProjectsSelect && typeof recentProjectsSelect.value === 'string'
        ? recentProjectsSelect.value.trim()
        : '';
    if (defaultProjectSelection && firstRunningProjectId) {
      setProjectIdentifier(defaultProjectSelection);
    }
    const shouldAutoFetch =
      defaultProjectSelection && (Boolean(firstRunningProjectId) || !existingProjectId);
    if (shouldAutoFetch) {
      fetchWorkflowStatus(false, defaultProjectSelection).catch(err =>
        console.error('Automatic workflow status fetch failed', err)
      );
    }
  } catch (err) {
    console.warn('Failed to load stored projects', err);
  }
}
