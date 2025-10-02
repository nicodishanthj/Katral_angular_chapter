import { postJSON, getJSON, escapeHtml, formatFileSize } from './utils.js';
import { markStepComplete, activatePanel } from './tabs.js';
import {
  setSelectedFlow,
  getSelectedFlow,
  onFlowChange,
  getFlowConfig,
  getKnowledgeFlows,
  getModernizationFlows,
  setProjectIdentifier,
  getProjectIdentifier,
  onProjectIdentifierChange
} from './state.js';
import {
  ensureWorkflowPolling,
  fetchWorkflowStatus,
  isWorkflowAlreadyRunningError,
  buildWorkflowStartPayload
} from './workflow.js';

const flowConfig = getFlowConfig();
const knowledgeFlows = getKnowledgeFlows();
const modernizationFlows = getModernizationFlows();

const projectConfigPresets = {
  'knowledge-base': {
    knowledge: {
      stackOptions: [
        { value: 'cobol', label: 'COBOL', selected: true },
        { value: 'jcl', label: 'JCL' },
        { value: 'ims', label: 'IMS' },
        { value: 'db2', label: 'DB2' }
      ],
      collectionOptions: [
        { value: 'cwa_docs', label: 'CWA Documentation', selected: true },
        { value: 'modernization_context', label: 'Modernization Context' },
        { value: 'legacy_reference', label: 'Legacy Reference' }
      ]
    }
  },
  'chat-with-code': {
    knowledge: {
      stackOptions: [
        { value: 'cobol', label: 'COBOL', selected: true },
        { value: 'pl1', label: 'PL/I' },
        { value: 'jcl', label: 'JCL' },
        { value: 'cics', label: 'CICS' }
      ],
      collectionOptions: [
        { value: 'chat_code', label: 'Chat Collections', selected: true },
        { value: 'mainframe_context', label: 'Mainframe Context' },
        { value: 'engineering_reviews', label: 'Engineering Reviews' }
      ]
    }
  },
  'doc-generation': {
    knowledge: {
      stackOptions: [
        { value: 'cobol', label: 'COBOL', selected: true },
        { value: 'jcl', label: 'JCL' },
        { value: 'ims', label: 'IMS' },
        { value: 'db2', label: 'DB2' }
      ],
      collectionOptions: [
        { value: 'legacy_docs', label: 'Legacy Documentation', selected: true },
        { value: 'application_summaries', label: 'Application Summaries' },
        { value: 'integration_maps', label: 'Integration Maps' }
      ]
    }
  },
  'code-conversion': {
    target: {
      languageOptions: [
        { value: 'java-21', label: 'Java 21 LTS', selected: true },
        { value: 'java-17', label: 'Java 17 LTS' },
        { value: 'java-11', label: 'Java 11' }
      ],
      versionOptions: [
        { value: 'openjdk-21', label: 'OpenJDK 21 LTS', selected: true },
        { value: 'openjdk-17', label: 'OpenJDK 17 LTS' },
        { value: 'temurin-11', label: 'Eclipse Temurin 11' }
      ],
      frameworkOptions: [
        { value: 'spring-boot', label: 'Spring Boot', selected: true },
        { value: 'spring-batch', label: 'Spring Batch' },
        { value: 'micronaut', label: 'Micronaut' },
        { value: 'quarkus', label: 'Quarkus' }
      ],
      runtimeOptions: [
        { value: 'aws-ecs-task', label: 'AWS ECS Task', selected: true },
        { value: 'aws-eks', label: 'AWS Elastic Kubernetes Service (EKS)' },
        { value: 'gcp-cloud-run', label: 'GCP Cloud Run' },
        { value: 'gke', label: 'Google Kubernetes Engine (GKE)' },
        { value: 'kubernetes', label: 'Kubernetes' }
      ]
    }
  }
};

let selectionCards = [];
let flowSummaryEl;
let knowledgeConfigSection;
let targetConfigSection;
let ingestIntroEl;
let knowledgeRepoInput;
let knowledgeCollectionInput;
let ingestProjectInput;
let knowledgeStacksDisplay;
let knowledgeStacksStatus;
let workflowStacksDisplay;
let workflowStacksStatus;
let workflowMainframeInput;
let workflowCollectionInput;
let ingestStatusEl;
let ingestButton;
let targetLanguageInput;
let targetVersionInput;
let targetFrameworkInput;
let targetRuntimeInput;
let targetNotesInput;
let targetConfigStatusEl;
let documentationFlowBlocks = [];
let ingestDropZone;
let ingestFileInput;
let ingestFileListEl;
let ingestFileSummaryEl;
let ingestFileItemsEl;
let ingestUploadButton;
let ingestClearButton;
let ingestUploadStatusEl;
let ingestUploadProgressEl;
let ingestUploadProgressFill;
let searchButton;
let searchQueryInput;
let searchResultsEl;

let detectedTechnologies = [];
let technologyStatusMessage = 'Select a repository to detect technologies.';
let technologyStatusLevel = 'info';
let technologyLoading = false;
let technologyRequestSeq = 0;
let lastDetectedRepo = '';
let queuedIngestFiles = [];
let uploadInFlight = false;

export function initIngest() {
  selectionCards = Array.from(document.querySelectorAll('.selection-card'));
  flowSummaryEl = document.getElementById('flowSelectionSummary');
  knowledgeConfigSection = document.getElementById('knowledgeConfigSection');
  targetConfigSection = document.getElementById('targetConfigSection');
  ingestIntroEl = document.getElementById('ingestIntro');
  knowledgeRepoInput = document.getElementById('ingestRepo');
  knowledgeCollectionInput = document.getElementById('ingestCollection');
  ingestProjectInput = document.getElementById('ingestProject');
  knowledgeStacksDisplay = document.getElementById('ingestStacksDisplay');
  knowledgeStacksStatus = document.getElementById('ingestStacksStatus');
  workflowStacksDisplay = document.getElementById('workflowStacksDisplay');
  workflowStacksStatus = document.getElementById('workflowStacksStatus');
  workflowMainframeInput = document.getElementById('workflowMainframe');
  workflowCollectionInput = document.getElementById('workflowCollection');
  ingestStatusEl = document.getElementById('ingestStatus');
  ingestButton = document.getElementById('ingestButton');
  targetLanguageInput = document.getElementById('targetLanguage');
  targetVersionInput = document.getElementById('targetVersion');
  targetFrameworkInput = document.getElementById('targetFramework');
  targetRuntimeInput = document.getElementById('targetRuntime');
  targetNotesInput = document.getElementById('targetNotes');
  targetConfigStatusEl = document.getElementById('targetConfigStatus');
  documentationFlowBlocks = Array.from(document.querySelectorAll('#docs [data-flows], #artifacts [data-flows]'));
  ingestDropZone = document.getElementById('ingestDropZone');
  ingestFileInput = document.getElementById('ingestFileInput');
  ingestFileListEl = document.getElementById('ingestFileList');
  ingestFileSummaryEl = document.getElementById('ingestFileSummary');
  ingestFileItemsEl = document.getElementById('ingestFileItems');
  ingestUploadButton = document.getElementById('ingestUploadButton');
  ingestClearButton = document.getElementById('ingestClearButton');
  ingestUploadStatusEl = document.getElementById('ingestUploadStatus');
  ingestUploadProgressEl = document.getElementById('ingestUploadProgress');
  ingestUploadProgressFill = document.getElementById('ingestUploadProgressFill');
  searchButton = document.getElementById('searchButton');
  searchQueryInput = document.getElementById('searchQuery');
  searchResultsEl = document.getElementById('searchResults');

  bindFlowSelection();
  bindKnowledgeInputs();
  bindTargetInputs();
  bindFileUpload();
  bindActions();
  bindProjectSync();
  bindSearch();
  bindWorkflowRepoSeed();

  renderTechnologyBadges();
  syncKnowledgeConfig();
  syncTargetConfig();
  updateIngestView(getSelectedFlow());
  updateDocumentationView(getSelectedFlow());

  const workflowMainframeValue = workflowMainframeInput ? workflowMainframeInput.value : '';
  if (workflowMainframeValue) {
    detectTechnologiesForRepo(workflowMainframeValue);
  }
}

function bindFlowSelection() {
  selectionCards.forEach(card => {
    card.addEventListener('click', () => {
      selectionCards.forEach(c => c.classList.remove('selected'));
      card.classList.add('selected');
      const flow = card.dataset.option || null;
      setSelectedFlow(flow);
      document.body.dataset.selectedFlow = flow || '';
      if (flowSummaryEl) {
        const title = card.querySelector('h3')?.textContent || flow;
        const summary = card.dataset.summary || '';
        flowSummaryEl.innerHTML = `<strong>${title}</strong>${summary ? ` &ndash; ${summary}` : ''}`;
        flowSummaryEl.hidden = false;
      }
      updateIngestView(flow);
      updateDocumentationView(flow);
      markStepComplete(1);
      activatePanel('ingest');
    });
  });

  onFlowChange(flow => {
    document.body.dataset.selectedFlow = flow || '';
    updateIngestView(flow);
    updateDocumentationView(flow);
  });

  const activeFlow = getSelectedFlow();
  if (activeFlow) {
    const activeCard = selectionCards.find(card => card.dataset.option === activeFlow);
    if (activeCard) {
      activeCard.classList.add('selected');
      document.body.dataset.selectedFlow = activeFlow;
    }
  }
}

function bindKnowledgeInputs() {
  if (knowledgeRepoInput) {
    knowledgeRepoInput.addEventListener('input', () => {
      flowConfig.knowledge.repo = knowledgeRepoInput.value;
      detectTechnologiesForRepo(knowledgeRepoInput.value);
    });
  }

  if (knowledgeCollectionInput) {
    knowledgeCollectionInput.addEventListener('change', () => {
      flowConfig.knowledge.collection = knowledgeCollectionInput.value.trim();
    });
  }

  if (workflowMainframeInput) {
    workflowMainframeInput.addEventListener('change', () => {
      detectTechnologiesForRepo(workflowMainframeInput.value);
    });
  }
}

function bindTargetInputs() {
  const selects = [targetLanguageInput, targetVersionInput, targetFrameworkInput, targetRuntimeInput];
  selects.forEach(select => {
    if (!select) {
      return;
    }
    select.addEventListener('change', () => {
      syncTargetConfig();
    });
  });

  if (targetNotesInput) {
    targetNotesInput.addEventListener('input', () => {
      syncTargetConfig();
    });
  }
}

function bindFileUpload() {
  if (ingestDropZone) {
    const preventDefaults = event => {
      event.preventDefault();
      event.stopPropagation();
    };
    ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
      ingestDropZone.addEventListener(eventName, preventDefaults);
    });
    ['dragenter', 'dragover'].forEach(eventName => {
      ingestDropZone.addEventListener(eventName, () => ingestDropZone.classList.add('dragover'));
    });
    ['dragleave', 'drop'].forEach(eventName => {
      ingestDropZone.addEventListener(eventName, () => ingestDropZone.classList.remove('dragover'));
    });
    ingestDropZone.addEventListener('drop', event => {
      if (event.dataTransfer?.files?.length) {
        addFilesToQueue(event.dataTransfer.files);
      }
    });
    ingestDropZone.addEventListener('click', () => {
      ingestFileInput?.click();
    });
    ingestDropZone.addEventListener('keydown', event => {
      if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault();
        ingestFileInput?.click();
      }
    });
  }

  if (ingestFileInput) {
    ingestFileInput.addEventListener('change', () => {
      addFilesToQueue(ingestFileInput.files);
    });
  }

  if (ingestUploadButton) {
    ingestUploadButton.addEventListener('click', () => {
      uploadQueuedFiles();
    });
  }

  if (ingestClearButton) {
    ingestClearButton.addEventListener('click', () => {
      if (uploadInFlight) {
        return;
      }
      clearQueuedFiles();
    });
  }

  renderQueuedFiles();
  resetUploadProgress();
}

function bindActions() {
  if (ingestButton) {
    ingestButton.addEventListener('click', async () => {
      syncKnowledgeConfig();
      const selectedFlow = getSelectedFlow();
      if (!knowledgeFlows.has(selectedFlow)) {
        if (ingestStatusEl) {
          ingestStatusEl.textContent =
            'Select a knowledge-enabled focus (Knowledge Base, Document Generation, Code Conversion, or Chat with Code) to run ingest.';
          ingestStatusEl.classList.add('warning');
        }
        return;
      }
      if (!ingestStatusEl) {
        return;
      }
      ingestStatusEl.classList.remove('warning');
      const activeProjectId = getProjectIdentifier();
      if (!activeProjectId) {
        ingestStatusEl.textContent =
          'Project identifier is required before ingesting sources. Add it above or in the workflow step.';
        ingestStatusEl.classList.add('warning');
        return;
      }
      setProjectIdentifier(activeProjectId);
      const trimmedRepoPath = (flowConfig.knowledge.repo || '').trim();
      if (!trimmedRepoPath) {
        ingestStatusEl.textContent =
          'Repository path is required when ingesting from an existing checkout. Use the file upload option if you do not have a local path.';
        ingestStatusEl.classList.add('warning');
        return;
      }
      ingestStatusEl.textContent = 'Ingesting...';
      try {
        const payload = {
          repo: flowConfig.knowledge.repo,
          stacks: flowConfig.knowledge.stacks,
          collection: flowConfig.knowledge.collection,
          project_id: activeProjectId
        };
        const response = await postJSON('/v1/ingest', payload);
        let text = `Indexed ${response.count} documents`;
        if (response.collection) {
          text += ` into collection ${response.collection}`;
        }
        if (response.warning) {
          text += ` (warning: ${response.warning})`;
        }
        ingestStatusEl.textContent = text;
        if (response.collection) {
          flowConfig.knowledge.collection = response.collection;
          if (knowledgeCollectionInput && knowledgeCollectionInput.value.trim() !== response.collection) {
            knowledgeCollectionInput.value = response.collection;
          }
          if (workflowCollectionInput && workflowCollectionInput.value.trim() !== response.collection) {
            workflowCollectionInput.value = response.collection;
          }
        }
        markStepComplete(2);
        activatePanel('docs', { allowLocked: true });
      } catch (err) {
        ingestStatusEl.textContent = 'Error: ' + err.message;
        ingestStatusEl.classList.add('warning');
      }
    });
  }

  if (targetConfigStatusEl) {
    targetConfigStatusEl.textContent = '';
  }

  const targetConfigApplyButton = document.getElementById('targetConfigApply');
  if (targetConfigApplyButton) {
    targetConfigApplyButton.addEventListener('click', async () => {
      syncTargetConfig();
      if (!targetConfigStatusEl) {
        return;
      }
      const selectedFlow = getSelectedFlow();
      if (!modernizationFlows.has(selectedFlow)) {
        targetConfigStatusEl.textContent = 'Select a modernization workflow to capture target settings.';
        targetConfigStatusEl.classList.add('warning');
        return;
      }
      targetConfigStatusEl.classList.remove('warning');
      targetConfigStatusEl.textContent = 'Target configuration saved for workflow kickoff.';

      if (selectedFlow === 'code-conversion') {
        let baseMessage = targetConfigStatusEl.textContent;
        try {
          syncKnowledgeConfig();
          const projectId = getProjectIdentifier();
          if (!projectId) {
            targetConfigStatusEl.textContent =
              baseMessage + ' Provide a project identifier to launch the workflow.';
            targetConfigStatusEl.classList.add('warning');
            return;
          }
          targetConfigStatusEl.textContent = baseMessage + ' Launching code conversion workflow...';
          const { payload, projectId: payloadProjectId } = buildWorkflowStartPayload();
          await postJSON('/v1/workflow/start', payload);
          targetConfigStatusEl.textContent = baseMessage + ' Code conversion workflow started.';
          markStepComplete(5);
          ensureWorkflowPolling(true);
          fetchWorkflowStatus(true, payloadProjectId);
        } catch (err) {
          if (isWorkflowAlreadyRunningError(err)) {
            targetConfigStatusEl.textContent = baseMessage + ' Code conversion workflow is already running.';
            ensureWorkflowPolling(true);
            const projectId = getProjectIdentifier();
            if (projectId) {
              fetchWorkflowStatus(true, projectId);
            }
          } else if (err && err.userFacing) {
            targetConfigStatusEl.textContent = baseMessage + ' ' + err.message;
            targetConfigStatusEl.classList.add('warning');
          } else {
            const message = err && err.message ? err.message : String(err);
            targetConfigStatusEl.textContent = baseMessage + ' Error launching workflow: ' + escapeHtml(message);
            targetConfigStatusEl.classList.add('warning');
          }
        }
      }
    });
  }
}

function bindProjectSync() {
  if (ingestProjectInput) {
    ingestProjectInput.addEventListener('input', () => {
      setProjectIdentifier(ingestProjectInput.value);
    });
    ingestProjectInput.addEventListener('change', () => {
      setProjectIdentifier(ingestProjectInput.value);
    });
  }

  onProjectIdentifierChange(projectId => {
    if (ingestProjectInput && ingestProjectInput.value.trim() !== projectId) {
      ingestProjectInput.value = projectId;
    }
  });
}

function bindSearch() {
  if (!searchButton || !searchQueryInput || !searchResultsEl) {
    return;
  }
  searchButton.addEventListener('click', async () => {
    const selectedFlow = getSelectedFlow();
    const query = searchQueryInput.value.trim();
    if (!knowledgeFlows.has(selectedFlow)) {
      searchResultsEl.textContent = 'Vector search is available after configuring a knowledge base.';
      return;
    }
    if (!query) {
      searchResultsEl.textContent = 'Enter a query first.';
      return;
    }
    searchResultsEl.textContent = 'Searching...';
    try {
      const projectId = getProjectIdentifier();
      let url = `/v1/search?q=${encodeURIComponent(query)}`;
      if (projectId) {
        url += `&project_id=${encodeURIComponent(projectId)}`;
      }
      const activeCollection = (flowConfig.knowledge.collection || '').trim();
      if (activeCollection) {
        url += `&collection=${encodeURIComponent(activeCollection)}`;
      }
      const response = await getJSON(url);
      if (!response.results || !response.results.length) {
        searchResultsEl.textContent = 'No results.';
        return;
      }
      searchResultsEl.innerHTML = response.results
        .map(pt => {
          const payload = pt.payload || {};
          const technologies = Array.isArray(payload.technologies) ? payload.technologies.join(', ') : payload.technologies || '';
          const techLine = technologies ? `<br/><em>Technologies: ${technologies}</em>` : '';
          const summary = payload.summary ? `<br/>${payload.summary}` : '';
          return `<div class="context-snippet"><strong>${payload.program || pt.id}</strong><br/>Score: ${pt.score}${techLine}${summary}<br/>${payload.content || ''}</div>`;
        })
        .join('');
    } catch (err) {
      searchResultsEl.textContent = 'Error: ' + err.message;
    }
  });
}

function bindWorkflowRepoSeed() {
  document.addEventListener('workflowRepoSeeded', event => {
    const repo = event.detail?.repo;
    if (typeof repo === 'string' && repo.trim()) {
      if (knowledgeRepoInput && !knowledgeRepoInput.value.trim()) {
        knowledgeRepoInput.value = repo;
      }
      flowConfig.knowledge.repo = repo;
      detectTechnologiesForRepo(repo, { force: true });
    }
  });
}

function updateIngestView(flow) {
  const showKnowledge = knowledgeFlows.has(flow);
  const showTarget = modernizationFlows.has(flow);
  if (knowledgeConfigSection) {
    knowledgeConfigSection.hidden = !showKnowledge;
  }
  if (targetConfigSection) {
    targetConfigSection.hidden = !showTarget;
  }
  if (ingestIntroEl) {
    ingestIntroEl.hidden = showKnowledge || showTarget;
  }
  if (ingestProjectInput) {
    ingestProjectInput.disabled = !showKnowledge;
  }
  if (knowledgeRepoInput) {
    knowledgeRepoInput.disabled = !showKnowledge;
  }
  if (knowledgeCollectionInput) {
    knowledgeCollectionInput.disabled = !showKnowledge;
  }
  if (targetLanguageInput) {
    targetLanguageInput.disabled = !showTarget;
  }
  if (targetVersionInput) {
    targetVersionInput.disabled = !showTarget;
  }
  if (targetFrameworkInput) {
    targetFrameworkInput.disabled = !showTarget;
  }
  if (targetRuntimeInput) {
    targetRuntimeInput.disabled = !showTarget;
  }
  if (targetNotesInput) {
    targetNotesInput.disabled = !showTarget;
  }
  if (showKnowledge) {
    const knowledgePresets = projectConfigPresets[flow]?.knowledge;
    configureSingleSelect(knowledgeCollectionInput, knowledgePresets?.collectionOptions || [], 'Select a collection');
    syncKnowledgeConfig();
    if (knowledgeRepoInput && knowledgeRepoInput.value.trim()) {
      detectTechnologiesForRepo(knowledgeRepoInput.value, { force: true });
    }
  }
  if (showTarget) {
    const targetPresets = projectConfigPresets[flow]?.target;
    configureSingleSelect(targetLanguageInput, targetPresets?.languageOptions || [], 'Select a language');
    configureSingleSelect(targetVersionInput, targetPresets?.versionOptions || [], 'Select a version');
    configureSingleSelect(targetFrameworkInput, targetPresets?.frameworkOptions || [], 'Select a framework');
    configureSingleSelect(targetRuntimeInput, targetPresets?.runtimeOptions || [], 'Select a runtime');
    syncTargetConfig();
  }
  if (targetConfigStatusEl) {
    targetConfigStatusEl.textContent = '';
    targetConfigStatusEl.classList.remove('warning');
  }
  if (ingestStatusEl) {
    ingestStatusEl.textContent = '';
    ingestStatusEl.classList.remove('warning');
  }
}

function updateDocumentationView(flow) {
  if (!documentationFlowBlocks.length) {
    return;
  }
  documentationFlowBlocks.forEach(block => {
    const flows = (block.dataset.flows || '')
      .split(',')
      .map(item => item.trim())
      .filter(Boolean);
    const hasArtifacts = block.dataset.hasArtifacts === 'true';
    const flowMatches = flows.length === 0 ? true : Boolean(flow && flows.includes(flow));
    const shouldShow = hasArtifacts || flowMatches;
    block.hidden = !shouldShow;
    if (shouldShow) {
      block.removeAttribute('aria-hidden');
    } else {
      block.setAttribute('aria-hidden', 'true');
    }
  });
}

function configureSingleSelect(selectEl, options, placeholder) {
  if (!selectEl) {
    return;
  }
  const isMultiple = selectEl.multiple;
  selectEl.innerHTML = '';
  if (!options || !options.length) {
    selectEl.disabled = true;
    if (placeholder && !isMultiple) {
      const placeholderOption = document.createElement('option');
      placeholderOption.value = '';
      placeholderOption.textContent = placeholder;
      placeholderOption.disabled = true;
      placeholderOption.selected = true;
      selectEl.appendChild(placeholderOption);
    }
    return;
  }
  selectEl.disabled = false;
  let hasSelected = false;
  if (placeholder && !isMultiple) {
    const placeholderOption = document.createElement('option');
    placeholderOption.value = '';
    placeholderOption.textContent = placeholder;
    placeholderOption.disabled = true;
    selectEl.appendChild(placeholderOption);
  }
  options.forEach(option => {
    const optionEl = document.createElement('option');
    optionEl.value = option.value;
    optionEl.textContent = option.label;
    if (option.selected) {
      optionEl.selected = true;
      hasSelected = true;
    }
    selectEl.appendChild(optionEl);
  });
  if (!hasSelected) {
    const defaultIndex = placeholder && !isMultiple ? 1 : 0;
    if (selectEl.options[defaultIndex]) {
      selectEl.options[defaultIndex].selected = true;
    }
  }
}

function renderTechnologyBadges() {
  const containers = [knowledgeStacksDisplay, workflowStacksDisplay];
  containers.forEach(container => {
    if (!container) {
      return;
    }
    container.innerHTML = '';
    if (!detectedTechnologies.length) {
      const placeholder = document.createElement('span');
      placeholder.className = 'tech-placeholder';
      placeholder.textContent = technologyLoading
        ? 'Detecting technologies…'
        : 'No technologies detected yet.';
      container.appendChild(placeholder);
      return;
    }
    detectedTechnologies.forEach(tech => {
      const badge = document.createElement('span');
      badge.className = 'tech-badge';
      badge.textContent = tech;
      container.appendChild(badge);
    });
  });
  const statusElements = [knowledgeStacksStatus, workflowStacksStatus];
  statusElements.forEach(statusEl => {
    if (!statusEl) {
      return;
    }
    const message = technologyStatusMessage;
    statusEl.textContent = message;
    statusEl.hidden = !message;
    statusEl.classList.toggle('warning', technologyStatusLevel === 'warning');
  });
}

async function detectTechnologiesForRepo(repoPath, options = {}) {
  const trimmed = (repoPath || '').trim();
  const force = Boolean(options.force);
  if (!force && trimmed !== '' && trimmed === lastDetectedRepo && !technologyLoading) {
    return;
  }
  technologyRequestSeq += 1;
  const requestId = technologyRequestSeq;
  if (!trimmed) {
    detectedTechnologies = [];
    lastDetectedRepo = '';
    technologyLoading = false;
    technologyStatusMessage = 'Select a repository to detect technologies.';
    technologyStatusLevel = 'info';
    renderTechnologyBadges();
    syncKnowledgeConfig();
    return;
  }
  technologyLoading = true;
  technologyStatusMessage = 'Detecting technologies…';
  technologyStatusLevel = 'info';
  renderTechnologyBadges();
  try {
    const response = await getJSON(`/v1/technologies?repo=${encodeURIComponent(trimmed)}`);
    if (requestId !== technologyRequestSeq) {
      return;
    }
    const list = Array.isArray(response.technologies)
      ? response.technologies.filter(value => typeof value === 'string' && value.trim() !== '')
      : [];
    detectedTechnologies = list;
    lastDetectedRepo = trimmed;
    if (detectedTechnologies.length === 0) {
      technologyStatusMessage = 'No technologies detected.';
      technologyStatusLevel = 'warning';
    } else {
      technologyStatusMessage = '';
      technologyStatusLevel = 'info';
    }
  } catch (err) {
    if (requestId !== technologyRequestSeq) {
      return;
    }
    detectedTechnologies = [];
    const message = err && err.message ? err.message : String(err);
    technologyStatusMessage = `Technology detection failed: ${message}`;
    technologyStatusLevel = 'warning';
  } finally {
    if (requestId === technologyRequestSeq) {
      technologyLoading = false;
      renderTechnologyBadges();
      syncKnowledgeConfig();
    }
  }
}

function syncKnowledgeConfig() {
  if (knowledgeRepoInput) {
    flowConfig.knowledge.repo = knowledgeRepoInput.value;
  } else {
    flowConfig.knowledge.repo = '';
  }
  if (knowledgeCollectionInput) {
    flowConfig.knowledge.collection = knowledgeCollectionInput.value.trim();
  } else {
    flowConfig.knowledge.collection = '';
  }
  flowConfig.knowledge.stacks = detectedTechnologies.slice();
}

function syncTargetConfig() {
  if (!targetLanguageInput || !targetVersionInput || !targetFrameworkInput || !targetRuntimeInput || !targetNotesInput) {
    return;
  }
  flowConfig.modernization.language = targetLanguageInput.value.trim();
  flowConfig.modernization.version = targetVersionInput.value.trim();
  flowConfig.modernization.framework = targetFrameworkInput.value.trim();
  flowConfig.modernization.runtime = targetRuntimeInput.value.trim();
  flowConfig.modernization.notes = targetNotesInput.value.trim();
}

function resetUploadProgress() {
  if (ingestUploadProgressEl) {
    ingestUploadProgressEl.hidden = true;
  }
  if (ingestUploadProgressFill) {
    ingestUploadProgressFill.style.width = '0%';
    ingestUploadProgressFill.setAttribute('aria-valuenow', '0');
  }
}

function renderQueuedFiles() {
  if (!ingestFileListEl || !ingestFileSummaryEl || !ingestFileItemsEl) {
    return;
  }
  ingestFileItemsEl.innerHTML = '';
  if (!queuedIngestFiles.length) {
    ingestFileListEl.hidden = true;
    ingestFileSummaryEl.textContent = 'No files selected yet.';
    return;
  }
  ingestFileListEl.hidden = false;
  const totalSize = queuedIngestFiles.reduce((acc, file) => acc + (file.size || 0), 0);
  const plural = queuedIngestFiles.length === 1 ? 'file' : 'files';
  ingestFileSummaryEl.textContent = `${queuedIngestFiles.length} ${plural} selected (${formatFileSize(totalSize)})`;
  const previewLimit = 20;
  queuedIngestFiles.slice(0, previewLimit).forEach(file => {
    const li = document.createElement('li');
    li.textContent = `${file.name} (${formatFileSize(file.size)})`;
    ingestFileItemsEl.appendChild(li);
  });
  if (queuedIngestFiles.length > previewLimit) {
    const li = document.createElement('li');
    li.textContent = `…and ${queuedIngestFiles.length - previewLimit} more`;
    ingestFileItemsEl.appendChild(li);
  }
}

function addFilesToQueue(fileList) {
  if (!fileList || !fileList.length) {
    return;
  }
  const existing = new Set(queuedIngestFiles.map(file => `${file.name}|${file.size}|${file.lastModified}`));
  Array.from(fileList).forEach(file => {
    const key = `${file.name}|${file.size}|${file.lastModified}`;
    if (!existing.has(key)) {
      queuedIngestFiles.push(file);
      existing.add(key);
    }
  });
  renderQueuedFiles();
  if (ingestUploadStatusEl) {
    ingestUploadStatusEl.textContent = '';
  }
}

function clearQueuedFiles() {
  queuedIngestFiles = [];
  if (ingestFileInput) {
    ingestFileInput.value = '';
  }
  renderQueuedFiles();
  resetUploadProgress();
  if (ingestUploadStatusEl) {
    ingestUploadStatusEl.textContent = 'Cleared selected files.';
  }
}

function setUploadControlsDisabled(disabled) {
  if (ingestUploadButton) {
    ingestUploadButton.disabled = disabled;
  }
  if (ingestClearButton) {
    ingestClearButton.disabled = disabled;
  }
  if (ingestDropZone) {
    ingestDropZone.setAttribute('aria-busy', disabled ? 'true' : 'false');
  }
}

function updateUploadProgress(current, total) {
  if (!ingestUploadProgressEl || !ingestUploadProgressFill) {
    return;
  }
  if (!total) {
    resetUploadProgress();
    return;
  }
  const percent = Math.min(100, Math.round((current / total) * 100));
  ingestUploadProgressEl.hidden = false;
  ingestUploadProgressFill.style.width = `${percent}%`;
  ingestUploadProgressFill.setAttribute('aria-valuenow', String(percent));
}

async function uploadQueuedFiles() {
  if (!queuedIngestFiles.length || uploadInFlight) {
    if (ingestUploadStatusEl && !queuedIngestFiles.length) {
      ingestUploadStatusEl.textContent = 'Add files to upload first.';
    }
    return;
  }
  syncKnowledgeConfig();
  uploadInFlight = true;
  setUploadControlsDisabled(true);
  updateUploadProgress(0, queuedIngestFiles.length);
  if (ingestUploadStatusEl) {
    ingestUploadStatusEl.textContent = `Uploading ${queuedIngestFiles.length} file${queuedIngestFiles.length === 1 ? '' : 's'}…`;
  }
  const trimmedRepoPath = (flowConfig.knowledge.repo || '').trim();
  const activeProjectId = getProjectIdentifier();
  if (!activeProjectId) {
    if (ingestUploadStatusEl) {
      ingestUploadStatusEl.textContent =
        'Project identifier is required before uploading files. Add it above or in the workflow step.';
    }
    uploadInFlight = false;
    setUploadControlsDisabled(false);
    resetUploadProgress();
    return;
  }
  setProjectIdentifier(activeProjectId);

  let lastIngestResponse = null;
  for (let index = 0; index < queuedIngestFiles.length; index += 1) {
    const file = queuedIngestFiles[index];
    const formData = new FormData();
    formData.append('files', file, file.name);
    if (flowConfig.knowledge.collection) {
      formData.append('collection', flowConfig.knowledge.collection);
    }
    if (trimmedRepoPath) {
      formData.append('repo', flowConfig.knowledge.repo);
    }
    if (activeProjectId) {
      formData.append('project_id', activeProjectId);
    }
    if (ingestUploadStatusEl) {
      ingestUploadStatusEl.textContent = `Uploading ${file.name} (${index + 1} of ${queuedIngestFiles.length})…`;
    }
    try {
      const response = await fetch('/v1/ingest/upload', {
        method: 'POST',
        body: formData
      });
      let payload = null;
      try {
        payload = await response.clone().json();
      } catch (parseErr) {
        payload = null;
      }
      if (!response.ok) {
        const message = payload && payload.error ? payload.error : response.statusText;
        throw new Error(message || 'Upload failed');
      }
      lastIngestResponse = payload;
    } catch (err) {
      if (ingestUploadStatusEl) {
        ingestUploadStatusEl.textContent = `Error uploading ${file.name}: ${err.message}`;
      }
      updateUploadProgress(index, queuedIngestFiles.length);
      uploadInFlight = false;
      setUploadControlsDisabled(false);
      return;
    }
    updateUploadProgress(index + 1, queuedIngestFiles.length);
  }
  if (ingestUploadStatusEl) {
    ingestUploadStatusEl.textContent = `Upload complete. ${queuedIngestFiles.length} file${queuedIngestFiles.length === 1 ? '' : 's'} uploaded successfully.`;
  }
  if (lastIngestResponse && lastIngestResponse.collection) {
    flowConfig.knowledge.collection = lastIngestResponse.collection;
    if (knowledgeCollectionInput && knowledgeCollectionInput.value.trim() !== lastIngestResponse.collection) {
      knowledgeCollectionInput.value = lastIngestResponse.collection;
    }
    if (workflowCollectionInput && workflowCollectionInput.value.trim() !== lastIngestResponse.collection) {
      workflowCollectionInput.value = lastIngestResponse.collection;
    }
  }
  queuedIngestFiles = [];
  renderQueuedFiles();
  setUploadControlsDisabled(false);
  resetUploadProgress();
  uploadInFlight = false;
}
