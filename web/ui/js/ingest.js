import { postJSON, getJSON, escapeHtml, formatFileSize } from './utils.js';
import { markStepComplete, activatePanel } from './tabs.js';
import {
  setSelectedFlow,
  getSelectedFlow,
  onFlowChange,
  getFlowConfig,
  getKnowledgeFlows,
  getModernizationFlows,
  getAngularMigrationFlows,
  getMigrationConfig,
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
const angularMigrationFlows = getAngularMigrationFlows();
const migrationConfig = getMigrationConfig();

const angularVersionCatalog = ['17', '16', '15', '14'];
const angularModuleOptions = [
  { value: 'standalone', label: 'Standalone Components' },
  { value: 'ngmodule', label: 'NgModule Based' },
  { value: 'hybrid', label: 'Hybrid (Standalone + NgModule)' }
];
const angularBuildSystems = [
  { value: 'angular-cli', label: 'Angular CLI' },
  { value: 'webpack', label: 'Webpack' },
  { value: 'nx', label: 'Nx Workspace' },
  { value: 'custom', label: 'Custom Toolchain' }
];
const reactVersionOptions = [
  { value: '19', label: 'React 19 (Release Candidate)' },
  { value: '18', label: 'React 18' },
  { value: '17', label: 'React 17' }
];
const reactComponentPatterns = [
  { value: 'functional-hooks', label: 'Functional Components with Hooks' },
  { value: 'class-components', label: 'Class Components' },
  { value: 'hybrid', label: 'Hybrid (Class + Functional)' }
];
const reactStateManagementOptions = [
  { value: 'redux-toolkit', label: 'Redux Toolkit' },
  { value: 'zustand', label: 'Zustand' },
  { value: 'recoil', label: 'Recoil' },
  { value: 'mobx', label: 'MobX' },
  { value: 'context-api', label: 'React Context + Hooks' }
];
const reactRoutingOptions = [
  { value: 'react-router', label: 'React Router' },
  { value: 'next-router', label: 'Next.js App Router' },
  { value: 'remix-router', label: 'Remix Router' },
  { value: 'tanstack-router', label: 'TanStack Router' },
  { value: 'custom', label: 'Custom / In-House' }
];
const reactUiLibraryOptions = [
  { value: 'mui', label: 'MUI' },
  { value: 'chakra', label: 'Chakra UI' },
  { value: 'antd', label: 'Ant Design' },
  { value: 'tailwind', label: 'Tailwind CSS' },
  { value: 'mantine', label: 'Mantine' },
  { value: 'headlessui', label: 'Headless UI' }
];

const migrationCollectionOptions = [
  { value: 'angular_patterns_collection', label: 'Angular Patterns Collection' },
  { value: 'react_patterns_collection', label: 'React Patterns Collection' },
  { value: 'migration_rules_mappings', label: 'Migration Rules and Mappings' },
  { value: 'best_practices_knowledge_base', label: 'Best Practices Knowledge Base' }
];

const angularKnowledgePreset = {
  stackOptions: [
    { value: 'angular-components', label: 'Angular Components', selected: true },
    { value: 'angular-services', label: 'Angular Services' },
    { value: 'angular-modules', label: 'Angular Modules' },
    { value: 'angular-routing', label: 'Angular Routing' },
    { value: 'angular-templates', label: 'Angular Templates' },
    { value: 'angular-cli', label: 'Angular CLI Tooling' },
    { value: 'rxjs-streams', label: 'RxJS Streams' },
    { value: 'ngrx-state', label: 'NgRx State Management' }
  ],
  versionOptions: [
    { value: 'angular-17', label: 'Angular v17 (Standalone)', selected: true },
    { value: 'angular-16', label: 'Angular v16' },
    { value: 'angular-15', label: 'Angular v15' },
    { value: 'angular-legacy', label: 'Angular v14 or earlier' }
  ],
  collectionOptions: [
    { value: 'angular_component_catalog', label: 'Angular Component Catalog', selected: true },
    { value: 'angular_service_registry', label: 'Angular Service Registry' },
    { value: 'angular_module_reference', label: 'Angular Module Reference' },
    ...migrationCollectionOptions
  ],
  additionalStacks: ['Standalone Components', 'Nx Workspaces', 'Jest Unit Tests'],
  useStackBadges: true
};

const angularTargetPreset = {
  languageOptions: [
    { value: 'typescript-5', label: 'TypeScript 5.x', selected: true },
    { value: 'typescript-4-9', label: 'TypeScript 4.9' },
    { value: 'typescript-4-8', label: 'TypeScript 4.8' }
  ],
  versionOptions: [
    { value: 'angular-17', label: 'Angular v17', selected: true },
    { value: 'angular-16', label: 'Angular v16' },
    { value: 'angular-15', label: 'Angular v15' },
    { value: 'angular-legacy', label: 'Angular v14 or earlier' }
  ],
  frameworkOptions: [
    { value: 'angular-standalone', label: 'Standalone Components', selected: true },
    { value: 'angular-ngmodule', label: 'NgModule Architecture' },
    { value: 'angular-hybrid', label: 'Hybrid (Standalone + NgModule)' },
    { value: 'angular-nx', label: 'Nx Monorepo Workspace' }
  ],
  runtimeOptions: [
    { value: 'angular-cli', label: 'Angular CLI', selected: true },
    { value: 'node-18', label: 'Node.js 18 LTS' },
    { value: 'cloud-run', label: 'Google Cloud Run' },
    { value: 'docker', label: 'Docker Container' }
  ]
};

const reactTargetPreset = {
  languageOptions: [
    { value: 'typescript-5', label: 'TypeScript 5.x', selected: true },
    { value: 'javascript-es2022', label: 'JavaScript (ES2022)' }
  ],
  versionOptions: [
    { value: 'typescript-5-4', label: 'TypeScript 5.4', selected: true },
    { value: 'typescript-4-9', label: 'TypeScript 4.9' },
    { value: 'javascript-es2020', label: 'JavaScript ES2020' }
  ],
  frameworkOptions: [
    { value: 'react-18', label: 'React 18 with Hooks', selected: true },
    { value: 'next-14', label: 'Next.js 14' },
    { value: 'remix', label: 'Remix' },
    { value: 'react-native', label: 'React Native' }
  ],
  runtimeOptions: [
    { value: 'vercel', label: 'Vercel Serverless', selected: true },
    { value: 'netlify-edge', label: 'Netlify Edge' },
    { value: 'aws-amplify', label: 'AWS Amplify' },
    { value: 'azure-static-web-apps', label: 'Azure Static Web Apps' },
    { value: 'docker-runtime', label: 'Docker (Containerized)' }
  ]
};

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
    knowledge: angularKnowledgePreset,
    target: reactTargetPreset
  },
  'angular-modernization': {
    knowledge: angularKnowledgePreset,
    target: angularTargetPreset
  },
  'angular-react-migration': {
    knowledge: angularKnowledgePreset,
    target: {
      ...reactTargetPreset
    }
  }
};

function hasKnowledgeConfiguration(flow) {
  return knowledgeFlows.has(flow) || Boolean(projectConfigPresets[flow]?.knowledge);
}

function hasTargetConfiguration(flow) {
  return modernizationFlows.has(flow) || Boolean(projectConfigPresets[flow]?.target);
}

function hasMigrationConfiguration(flow) {
  return angularMigrationFlows.has(flow);
}

function getKnowledgePreset(flow) {
  return projectConfigPresets[flow]?.knowledge || null;
}

function getTargetPreset(flow) {
  return projectConfigPresets[flow]?.target || null;
}

let presetTechnologyBadges = [];

function updatePresetTechnologiesForFlow(flow) {
  if (!flow) {
    presetTechnologyBadges = [];
    return;
  }
  const knowledgePreset = getKnowledgePreset(flow);
  if (!knowledgePreset) {
    presetTechnologyBadges = [];
    return;
  }
  const badges = [];
  const addOptionLabels = options => {
    if (!Array.isArray(options)) {
      return;
    }
    options.forEach(option => {
      const label = typeof option?.label === 'string' && option.label.trim() ? option.label.trim() : null;
      if (label) {
        badges.push(label);
      }
    });
  };
  addOptionLabels(knowledgePreset.versionOptions);
  if (knowledgePreset.useStackBadges) {
    addOptionLabels(knowledgePreset.stackOptions);
  }
  if (Array.isArray(knowledgePreset.additionalStacks)) {
    knowledgePreset.additionalStacks
      .filter(item => typeof item === 'string' && item.trim())
      .forEach(item => badges.push(item.trim()));
  }
  const unique = new Set();
  presetTechnologyBadges = badges.filter(item => {
    if (unique.has(item)) {
      return false;
    }
    unique.add(item);
    return true;
  });
}

let selectionCards = [];
let flowSummaryEl;
let knowledgeConfigSection;
let targetConfigSection;
let migrationConfigSection;
let angularVersionSelect;
let angularVersionDetectedEl;
let angularModuleTypeSelect;
let angularBuildSystemSelect;
let reactVersionSelect;
let reactComponentPatternSelect;
let reactStateManagementSelect;
let reactRoutingSelect;
let reactUiLibrariesSelect;
let componentLifecycleMappingsInput;
let directiveConversionMappingsInput;
let serviceContextMappingsInput;
let pipeConversionMappingsInput;
let guardRouteMappingsInput;
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
let detectedAngularVersion = '';
let queuedIngestFiles = [];

function normalizeRelativePath(rawPath) {
  if (!rawPath || typeof rawPath !== 'string') {
    return '';
  }
  return rawPath
    .replace(/\\/g, '/')
    .replace(/^(\.\/)+/, '')
    .replace(/^\/+/g, '');
}

function createFileEntry(file, relativePath = '') {
  if (!file) {
    return null;
  }
  const candidatePath = relativePath || file.webkitRelativePath || file.relativePath || '';
  const normalizedPath = normalizeRelativePath(candidatePath);
  return {
    file,
    relativePath: normalizedPath
  };
}

function getFileDisplayName(fileEntry) {
  if (!fileEntry || !fileEntry.file) {
    return '';
  }
  const { file, relativePath } = fileEntry;
  return relativePath || file?.name || '';
}

function getFileQueueKey(fileEntry) {
  if (!fileEntry || !fileEntry.file) {
    return '';
  }
  const { file, relativePath } = fileEntry;
  const normalizedPath = normalizeRelativePath(
    relativePath || file.webkitRelativePath || file.relativePath || ''
  );
  const pathComponent = normalizedPath || file.name || '';
  return `${pathComponent}|${file.name}|${file.size}|${file.lastModified}`;
}

async function gatherFilesFromDataTransfer(dataTransfer) {
  if (!dataTransfer) {
    return [];
  }

  if (!dataTransfer.items || !dataTransfer.items.length) {
    return Array.from(dataTransfer.files || []).map(file => createFileEntry(file)).filter(Boolean);
  }

  const entries = Array.from(dataTransfer.items)
    .map(item => (typeof item.webkitGetAsEntry === 'function' ? item.webkitGetAsEntry() : null))
    .filter(Boolean);

  if (!entries.length) {
    return Array.from(dataTransfer.files || []).map(file => createFileEntry(file)).filter(Boolean);
  }

  const walkEntry = async (entry, pathPrefix = '') => {
    if (!entry) {
      return [];
    }
    const entryName = entry.name || '';
    if (entry.isFile) {
      return new Promise(resolve => {
        entry.file(file => {
          const relativePath = pathPrefix ? `${pathPrefix}/${file.name}` : file.name;
          const entryWithPath = createFileEntry(file, relativePath);
          resolve(entryWithPath ? [entryWithPath] : []);
        }, () => {
          resolve([]);
        });
      });
    }
    if (entry.isDirectory) {
      const directoryPath = pathPrefix ? `${pathPrefix}/${entryName}` : entryName;
      const reader = entry.createReader();
      const readAllEntries = () =>
        new Promise((resolve, reject) => {
          const results = [];
          const readBatch = () => {
            reader.readEntries(batch => {
              if (!batch.length) {
                resolve(results);
                return;
              }
              results.push(...batch);
              readBatch();
            }, err => {
              reject(err);
            });
          };
          readBatch();
        });
      try {
        const childEntries = await readAllEntries();
        const childResults = await Promise.all(
          childEntries.map(child => walkEntry(child, directoryPath))
        );
        return childResults.flat();
      } catch (err) {
        return [];
      }
    }
    return [];
  };

  const files = await Promise.all(entries.map(entry => walkEntry(entry)));
  const flattened = files.flat().filter(Boolean);
  if (flattened.length) {
    return flattened;
  }
  return Array.from(dataTransfer.files || []).map(file => createFileEntry(file)).filter(Boolean);
}
let uploadInFlight = false;

export function initIngest() {
  selectionCards = Array.from(document.querySelectorAll('.selection-card'));
  flowSummaryEl = document.getElementById('flowSelectionSummary');
  knowledgeConfigSection = document.getElementById('knowledgeConfigSection');
  targetConfigSection = document.getElementById('targetConfigSection');
  migrationConfigSection = document.getElementById('migrationConfigSection');
  const modernizationTitle = 'Katral - Angular Modernization Chapter';
  document.title = modernizationTitle;
  const headerTitle = document.querySelector('header h1');
  if (headerTitle) {
    headerTitle.textContent = modernizationTitle;
  }
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
  angularVersionSelect = document.getElementById('angularVersion');
  angularVersionDetectedEl = document.getElementById('angularVersionDetected');
  angularModuleTypeSelect = document.getElementById('angularModuleType');
  angularBuildSystemSelect = document.getElementById('angularBuildSystem');
  reactVersionSelect = document.getElementById('reactVersion');
  reactComponentPatternSelect = document.getElementById('reactComponentPattern');
  reactStateManagementSelect = document.getElementById('reactStateManagement');
  reactRoutingSelect = document.getElementById('reactRouting');
  reactUiLibrariesSelect = document.getElementById('reactUiLibraries');
  componentLifecycleMappingsInput = document.getElementById('componentLifecycleMappings');
  directiveConversionMappingsInput = document.getElementById('directiveConversionMappings');
  serviceContextMappingsInput = document.getElementById('serviceContextMappings');
  pipeConversionMappingsInput = document.getElementById('pipeConversionMappings');
  guardRouteMappingsInput = document.getElementById('guardRouteMappings');
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
  bindMigrationInputs();
  bindFileUpload();
  bindActions();
  bindProjectSync();
  bindSearch();
  bindWorkflowRepoSeed();

  renderMigrationMappings();
  renderTechnologyBadges();
  syncKnowledgeConfig();
  syncTargetConfig();
  syncMigrationConfig();
  updateIngestView(getSelectedFlow());
  updateDocumentationView(getSelectedFlow());
  updateAngularVersionMessaging();

  const workflowMainframeValue = workflowMainframeInput ? workflowMainframeInput.value : '';
  if (workflowMainframeValue) {
    detectTechnologiesForRepo(workflowMainframeValue);
  }
}

function normalizeFlowValue(flow) {
  return typeof flow === 'string' && flow.trim() ? flow.trim() : '';
}

function renderSelectedFlow(flow) {
  const normalized = normalizeFlowValue(flow);
  let matchedCard = null;

  selectionCards.forEach(card => {
    const optionValue = normalizeFlowValue(card.dataset.option);
    const isMatch = normalized && optionValue === normalized;
    card.classList.toggle('selected', isMatch);
    card.setAttribute('aria-pressed', isMatch ? 'true' : 'false');
    if (isMatch) {
      matchedCard = card;
    }
  });

  document.body.dataset.selectedFlow = normalized;

  if (!flowSummaryEl) {
    return;
  }

  if (matchedCard) {
    const title = matchedCard.querySelector('h3')?.textContent?.trim() || normalized;
    const summaryRaw = matchedCard.dataset.summary || '';
    const summary = summaryRaw.trim();
    flowSummaryEl.innerHTML = `<strong>${title}</strong>${summary ? ` &ndash; ${summary}` : ''}`;
    flowSummaryEl.hidden = false;
  } else {
    flowSummaryEl.innerHTML = '';
    flowSummaryEl.hidden = true;
  }
}

function applyFlowSelection(flow) {
  renderSelectedFlow(flow);
  updateIngestView(flow);
  updateDocumentationView(flow);
}

function bindFlowSelection() {
  selectionCards.forEach(card => {
    card.addEventListener('click', () => {
      const optionValue = normalizeFlowValue(card.dataset.option);
      const flow = optionValue || null;
      applyFlowSelection(flow);
      const appliedFlow = setSelectedFlow(flow);
      markStepComplete(1);
      if (hasTargetConfiguration(appliedFlow)) {
        activatePanel('chat');
      } else {
        activatePanel('ingest');
      }
    });
  });

  onFlowChange(flow => {
    applyFlowSelection(flow);
  });

  applyFlowSelection(getSelectedFlow());
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

function bindMigrationInputs() {
  const selects = [
    angularVersionSelect,
    angularModuleTypeSelect,
    angularBuildSystemSelect,
    reactVersionSelect,
    reactComponentPatternSelect,
    reactStateManagementSelect,
    reactRoutingSelect
  ];
  selects.forEach(select => {
    if (!select) {
      return;
    }
    select.addEventListener('change', () => {
      syncMigrationConfig();
    });
  });

  if (reactUiLibrariesSelect) {
    reactUiLibrariesSelect.addEventListener('change', () => {
      syncMigrationConfig();
    });
  }

  const mappingInputs = [
    [componentLifecycleMappingsInput, 'componentLifecycle'],
    [directiveConversionMappingsInput, 'directiveConversions'],
    [serviceContextMappingsInput, 'serviceContexts'],
    [pipeConversionMappingsInput, 'pipeConversions'],
    [guardRouteMappingsInput, 'guardRoutes']
  ];
  mappingInputs.forEach(([input]) => {
    if (!input) {
      return;
    }
    input.addEventListener('input', () => {
      syncMigrationConfig();
    });
    input.addEventListener('blur', () => {
      syncMigrationConfig();
    });
  });
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
    ingestDropZone.addEventListener('drop', async event => {
      if (!event.dataTransfer) {
        return;
      }
      const files = await gatherFilesFromDataTransfer(event.dataTransfer);
      if (files.length) {
        addFilesToQueue(files);
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
      const files = Array.from(ingestFileInput.files || [])
        .map(file => createFileEntry(file))
        .filter(Boolean);
      addFilesToQueue(files);
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
      if (!hasKnowledgeConfiguration(selectedFlow)) {
        if (ingestStatusEl) {
          ingestStatusEl.textContent =
            'Select a knowledge-enabled focus (Knowledge Base, Document Generation, Angular Modernization, Angular to React Migration, Code Conversion, or Chat with Code) to run ingest.';
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
      if (!hasTargetConfiguration(selectedFlow)) {
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
    if (!hasKnowledgeConfiguration(selectedFlow)) {
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
  const knowledgePreset = getKnowledgePreset(flow);
  const targetPreset = getTargetPreset(flow);
  const showKnowledge = hasKnowledgeConfiguration(flow);
  const showTarget = hasTargetConfiguration(flow);
  const showMigration = hasMigrationConfiguration(flow);
  if (knowledgeConfigSection) {
    knowledgeConfigSection.hidden = !showKnowledge;
  }
  if (targetConfigSection) {
    targetConfigSection.hidden = !showTarget;
  }
  if (migrationConfigSection) {
    migrationConfigSection.hidden = !showMigration;
  }
  if (ingestIntroEl) {
    ingestIntroEl.hidden = showKnowledge || showTarget || showMigration;
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
  const migrationSelects = [
    angularVersionSelect,
    angularModuleTypeSelect,
    angularBuildSystemSelect,
    reactVersionSelect,
    reactComponentPatternSelect,
    reactStateManagementSelect,
    reactRoutingSelect,
    reactUiLibrariesSelect
  ];
  migrationSelects.forEach(select => {
    if (select) {
      select.disabled = !showMigration;
    }
  });

  updatePresetTechnologiesForFlow(showKnowledge ? flow : null);
  if (!showKnowledge && !technologyLoading) {
    technologyStatusMessage = 'Select a repository to detect technologies.';
    technologyStatusLevel = 'info';
  }
  renderTechnologyBadges();

  if (showKnowledge) {
    configureSingleSelect(knowledgeCollectionInput, knowledgePreset?.collectionOptions || [], 'Select a collection');
  } else if (knowledgeCollectionInput) {
    knowledgeCollectionInput.innerHTML = '';
  }

  if (showTarget) {
    configureSingleSelect(targetLanguageInput, targetPreset?.languageOptions || [], 'Select a language');
    configureSingleSelect(targetVersionInput, targetPreset?.versionOptions || [], 'Select a version');
    configureSingleSelect(targetFrameworkInput, targetPreset?.frameworkOptions || [], 'Select a framework');
    configureSingleSelect(targetRuntimeInput, targetPreset?.runtimeOptions || [], 'Select a runtime');
  }

  if (showMigration) {
    configureMigrationSelects();
  }

  syncKnowledgeConfig();
  syncTargetConfig();
  if (showMigration) {
    renderMigrationMappings();
  }
  syncMigrationConfig();
  updateAngularVersionMessaging();

  if (showKnowledge && knowledgeRepoInput && knowledgeRepoInput.value.trim()) {
    detectTechnologiesForRepo(knowledgeRepoInput.value, { force: true });
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

function configureMigrationSelects() {
  if (!migrationConfig) {
    return;
  }
  const source = migrationConfig.source || {};
  const target = migrationConfig.target || {};
  const selectedAngularVersion = source.version || 'auto';
  const autoLabel = detectedAngularVersion
    ? `Use detected version (Angular v${detectedAngularVersion})`
    : 'Use detected version (pending detection)';
  const versionOptions = [
    { value: 'auto', label: autoLabel, selected: selectedAngularVersion === 'auto' || selectedAngularVersion === '' }
  ];
  angularVersionCatalog.forEach(version => {
    versionOptions.push({
      value: version,
      label: `Angular v${version}`,
      selected: selectedAngularVersion === version
    });
  });
  configureSingleSelect(angularVersionSelect, versionOptions);

  const moduleValue = source.moduleType || 'standalone';
  const moduleOptions = angularModuleOptions.map(option => ({
    value: option.value,
    label: option.label,
    selected: moduleValue === option.value
  }));
  configureSingleSelect(angularModuleTypeSelect, moduleOptions);

  const buildValue = source.buildSystem || 'angular-cli';
  const buildOptions = angularBuildSystems.map(option => ({
    value: option.value,
    label: option.label,
    selected: buildValue === option.value
  }));
  configureSingleSelect(angularBuildSystemSelect, buildOptions);

  const reactVersionValue = target.version || '18';
  const reactVersionList = reactVersionOptions.map(option => ({
    value: option.value,
    label: option.label,
    selected: reactVersionValue === option.value
  }));
  configureSingleSelect(reactVersionSelect, reactVersionList);

  const componentValue = target.componentPattern || 'functional-hooks';
  const componentOptions = reactComponentPatterns.map(option => ({
    value: option.value,
    label: option.label,
    selected: componentValue === option.value
  }));
  configureSingleSelect(reactComponentPatternSelect, componentOptions);

  const stateValue = target.stateManagement || 'redux-toolkit';
  const stateOptions = reactStateManagementOptions.map(option => ({
    value: option.value,
    label: option.label,
    selected: stateValue === option.value
  }));
  configureSingleSelect(reactStateManagementSelect, stateOptions);

  const routingValue = target.routing || 'react-router';
  const routingOptions = reactRoutingOptions.map(option => ({
    value: option.value,
    label: option.label,
    selected: routingValue === option.value
  }));
  configureSingleSelect(reactRoutingSelect, routingOptions);

  const selectedLibraries = new Set((target.uiLibraries || []).map(value => value || '').filter(Boolean));
  const libraryOptions = reactUiLibraryOptions.map(option => ({
    value: option.value,
    label: option.label,
    selected: selectedLibraries.has(option.value)
  }));
  configureSingleSelect(reactUiLibrariesSelect, libraryOptions);
}

function renderMigrationMappings() {
  if (!migrationConfig || !migrationConfig.mappingText) {
    return;
  }
  const mappingText = migrationConfig.mappingText;
  if (componentLifecycleMappingsInput) {
    componentLifecycleMappingsInput.value = mappingText.componentLifecycle || '';
  }
  if (directiveConversionMappingsInput) {
    directiveConversionMappingsInput.value = mappingText.directiveConversions || '';
  }
  if (serviceContextMappingsInput) {
    serviceContextMappingsInput.value = mappingText.serviceContexts || '';
  }
  if (pipeConversionMappingsInput) {
    pipeConversionMappingsInput.value = mappingText.pipeConversions || '';
  }
  if (guardRouteMappingsInput) {
    guardRouteMappingsInput.value = mappingText.guardRoutes || '';
  }
}

function updateAngularVersionMessaging() {
  if (angularVersionDetectedEl) {
    if (detectedAngularVersion) {
      angularVersionDetectedEl.textContent = `Auto-detected Angular version: v${detectedAngularVersion}. Override if the repository uses a different baseline.`;
    } else {
      angularVersionDetectedEl.textContent = 'Angular version will auto-detect once technologies are scanned.';
    }
  }
  if (!angularVersionSelect) {
    return;
  }
  const autoOption = Array.from(angularVersionSelect.options || []).find(option => option.value === 'auto');
  if (autoOption) {
    autoOption.textContent = detectedAngularVersion
      ? `Use detected version (Angular v${detectedAngularVersion})`
      : 'Use detected version (pending detection)';
  }
}

function renderTechnologyBadges() {
  const combined = [];
  const seen = new Set();
  const pushBadge = value => {
    const label = typeof value === 'string' ? value.trim() : '';
    if (!label || seen.has(label)) {
      return;
    }
    seen.add(label);
    combined.push(label);
  };
  detectedTechnologies.forEach(pushBadge);
  presetTechnologyBadges.forEach(pushBadge);

  const containers = [knowledgeStacksDisplay, workflowStacksDisplay];
  containers.forEach(container => {
    if (!container) {
      return;
    }
    container.innerHTML = '';
    if (!combined.length) {
      const placeholder = document.createElement('span');
      placeholder.className = 'tech-placeholder';
      placeholder.textContent = technologyLoading
        ? 'Detecting technologies…'
        : 'No technologies detected yet.';
      container.appendChild(placeholder);
      return;
    }
    combined.forEach(tech => {
      const badge = document.createElement('span');
      badge.className = 'tech-badge';
      badge.textContent = tech;
      container.appendChild(badge);
    });
  });
  const usingPresetOnly = !detectedTechnologies.length && presetTechnologyBadges.length > 0;
  const statusElements = [knowledgeStacksStatus, workflowStacksStatus];
  let message = technologyStatusMessage;
  let level = technologyStatusLevel;
  if (usingPresetOnly && !technologyLoading && technologyStatusLevel !== 'warning') {
    message = 'Using Angular stack presets. Provide a repository to refine detection.';
    level = 'info';
  }
  statusElements.forEach(statusEl => {
    if (!statusEl) {
      return;
    }
    statusEl.textContent = message;
    statusEl.hidden = !message;
    statusEl.classList.toggle('warning', level === 'warning');
  });
  if (usingPresetOnly && !technologyLoading && technologyStatusLevel !== 'warning') {
    technologyStatusMessage = message;
    technologyStatusLevel = level;
  }
}

function extractAngularVersion(value) {
  if (typeof value !== 'string') {
    return null;
  }
  const normalized = value.trim();
  if (!normalized) {
    return null;
  }
  const directMatch = normalized.match(/(?:^|[^a-z])angular(?!js)[\s@-]*(?:v(?:ersion)?\.?)?\s*(\d+(?:\.\d+)*)/i);
  if (directMatch && directMatch[1]) {
    return directMatch[1];
  }
  const scopedMatch = normalized.match(/@angular\/(?:core|cli|common|platform-browser)@?(\d+(?:\.\d+)*)/i);
  if (scopedMatch && scopedMatch[1]) {
    return scopedMatch[1];
  }
  return null;
}

function enrichDetectedTechnologies(list, repoPath) {
  const enriched = [];
  const seen = new Set();
  const add = value => {
    const label = typeof value === 'string' ? value.trim() : '';
    if (!label || seen.has(label)) {
      return;
    }
    seen.add(label);
    enriched.push(label);
  };
  let angularDetected = false;
  let angularVersion = null;
  (list || []).forEach(value => {
    add(value);
    if (typeof value === 'string') {
      const version = extractAngularVersion(value);
      if (version && !angularVersion) {
        angularVersion = version;
      }
      if (/angular(?!js)/i.test(value) || /@angular\//i.test(value)) {
        angularDetected = true;
      }
    }
  });
  if (!angularDetected && typeof repoPath === 'string' && /angular/i.test(repoPath)) {
    angularDetected = true;
  }
  if (!angularDetected) {
    const lower = (list || []).map(value => (typeof value === 'string' ? value.toLowerCase() : ''));
    if (lower.some(value => value.includes('@angular/'))) {
      angularDetected = true;
    } else if (lower.includes('typescript') && lower.includes('rxjs')) {
      angularDetected = true;
    }
  }
  if (angularDetected) {
    const versionLabels = angularVersion
      ? [`Angular v${angularVersion}`]
      : ['Angular v17', 'Angular v16', 'Angular v15'];
    versionLabels.forEach(add);
    [
      'Angular Components',
      'Angular Services',
      'Angular Modules',
      'Angular Routing',
      'Angular Templates',
      'Angular CLI Tooling',
      'RxJS Streams',
      'NgRx State Management'
    ].forEach(add);
  }
  detectedAngularVersion = angularDetected ? angularVersion || '' : '';
  return enriched;
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
    detectedAngularVersion = '';
    lastDetectedRepo = '';
    technologyLoading = false;
    technologyStatusMessage = 'Select a repository to detect technologies.';
    technologyStatusLevel = 'info';
    renderTechnologyBadges();
    syncKnowledgeConfig();
    syncMigrationConfig();
    updateAngularVersionMessaging();
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
    detectedTechnologies = enrichDetectedTechnologies(list, trimmed);
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
    detectedAngularVersion = '';
    const message = err && err.message ? err.message : String(err);
    technologyStatusMessage = `Technology detection failed: ${message}`;
    technologyStatusLevel = 'warning';
  } finally {
    if (requestId === technologyRequestSeq) {
      technologyLoading = false;
      renderTechnologyBadges();
      syncKnowledgeConfig();
      syncMigrationConfig();
      updateAngularVersionMessaging();
    }
  }
}

function syncKnowledgeConfig() {
  const activeFlow = getSelectedFlow();
  const knowledgeEnabled = hasKnowledgeConfiguration(activeFlow);
  if (knowledgeEnabled && knowledgeRepoInput) {
    flowConfig.knowledge.repo = knowledgeRepoInput.value;
  } else {
    flowConfig.knowledge.repo = '';
  }
  if (knowledgeEnabled && knowledgeCollectionInput) {
    flowConfig.knowledge.collection = knowledgeCollectionInput.value.trim();
  } else {
    flowConfig.knowledge.collection = '';
  }
  if (knowledgeEnabled) {
    const stacks = [];
    const seen = new Set();
    const add = value => {
      const label = typeof value === 'string' ? value.trim() : '';
      if (!label || seen.has(label)) {
        return;
      }
      seen.add(label);
      stacks.push(label);
    };
    detectedTechnologies.forEach(add);
    presetTechnologyBadges.forEach(add);
    flowConfig.knowledge.stacks = stacks;
  } else {
    flowConfig.knowledge.stacks = [];
  }
}

function syncTargetConfig() {
  if (!targetLanguageInput || !targetVersionInput || !targetFrameworkInput || !targetRuntimeInput || !targetNotesInput) {
    return;
  }
  const activeFlow = getSelectedFlow();
  const targetEnabled = hasTargetConfiguration(activeFlow);
  if (!targetEnabled) {
    flowConfig.modernization.language = '';
    flowConfig.modernization.version = '';
    flowConfig.modernization.framework = '';
    flowConfig.modernization.runtime = '';
    flowConfig.modernization.notes = '';
    return;
  }
  flowConfig.modernization.language = targetLanguageInput.value.trim();
  flowConfig.modernization.version = targetVersionInput.value.trim();
  flowConfig.modernization.framework = targetFrameworkInput.value.trim();
  flowConfig.modernization.runtime = targetRuntimeInput.value.trim();
  flowConfig.modernization.notes = targetNotesInput.value.trim();
}

function syncMigrationConfig() {
  if (!migrationConfig) {
    return;
  }
  const source = migrationConfig.source || (migrationConfig.source = {});
  const target = migrationConfig.target || (migrationConfig.target = {});
  const mappingText = migrationConfig.mappingText || (migrationConfig.mappingText = {});
  source.detectedVersion = detectedAngularVersion || '';
  if (angularVersionSelect) {
    const value = angularVersionSelect.value || 'auto';
    source.version = value;
  } else if (!source.version) {
    source.version = 'auto';
  }
  if (angularModuleTypeSelect) {
    source.moduleType = angularModuleTypeSelect.value || 'standalone';
  } else if (!source.moduleType) {
    source.moduleType = 'standalone';
  }
  if (angularBuildSystemSelect) {
    source.buildSystem = angularBuildSystemSelect.value || 'angular-cli';
  } else if (!source.buildSystem) {
    source.buildSystem = 'angular-cli';
  }

  if (reactVersionSelect) {
    target.version = reactVersionSelect.value || '18';
  } else if (!target.version) {
    target.version = '18';
  }
  if (reactComponentPatternSelect) {
    target.componentPattern = reactComponentPatternSelect.value || 'functional-hooks';
  } else if (!target.componentPattern) {
    target.componentPattern = 'functional-hooks';
  }
  if (reactStateManagementSelect) {
    target.stateManagement = reactStateManagementSelect.value || 'redux-toolkit';
  } else if (!target.stateManagement) {
    target.stateManagement = 'redux-toolkit';
  }
  if (reactRoutingSelect) {
    target.routing = reactRoutingSelect.value || 'react-router';
  } else if (!target.routing) {
    target.routing = 'react-router';
  }
  if (reactUiLibrariesSelect) {
    const libraries = Array.from(reactUiLibrariesSelect.selectedOptions || [])
      .map(option => option.value)
      .filter(Boolean);
    target.uiLibraries = libraries;
  } else if (!Array.isArray(target.uiLibraries)) {
    target.uiLibraries = [];
  }

  if (componentLifecycleMappingsInput) {
    mappingText.componentLifecycle = normalizeMappingText(componentLifecycleMappingsInput.value);
  } else if (!mappingText.componentLifecycle) {
    mappingText.componentLifecycle = '';
  }
  if (directiveConversionMappingsInput) {
    mappingText.directiveConversions = normalizeMappingText(directiveConversionMappingsInput.value);
  } else if (!mappingText.directiveConversions) {
    mappingText.directiveConversions = '';
  }
  if (serviceContextMappingsInput) {
    mappingText.serviceContexts = normalizeMappingText(serviceContextMappingsInput.value);
  } else if (!mappingText.serviceContexts) {
    mappingText.serviceContexts = '';
  }
  if (pipeConversionMappingsInput) {
    mappingText.pipeConversions = normalizeMappingText(pipeConversionMappingsInput.value);
  } else if (!mappingText.pipeConversions) {
    mappingText.pipeConversions = '';
  }
  if (guardRouteMappingsInput) {
    mappingText.guardRoutes = normalizeMappingText(guardRouteMappingsInput.value);
  } else if (!mappingText.guardRoutes) {
    mappingText.guardRoutes = '';
  }
}

function normalizeMappingText(value) {
  if (typeof value !== 'string') {
    return '';
  }
  return value
    .replace(/\r\n/g, '\n')
    .split('\n')
    .map(line => line.trim())
    .join('\n')
    .trim();
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
  const totalSize = queuedIngestFiles.reduce((acc, fileEntry) => acc + (fileEntry.file?.size || 0), 0);
  const plural = queuedIngestFiles.length === 1 ? 'file' : 'files';
  ingestFileSummaryEl.textContent = `${queuedIngestFiles.length} ${plural} selected (${formatFileSize(totalSize)})`;
  const previewLimit = 20;
  queuedIngestFiles.slice(0, previewLimit).forEach(fileEntry => {
    const li = document.createElement('li');
    const displayName = getFileDisplayName(fileEntry) || fileEntry.file?.name || 'Unnamed file';
    const fileSize = fileEntry.file?.size || 0;
    li.textContent = `${displayName} (${formatFileSize(fileSize)})`;
    if (fileEntry.relativePath) {
      li.title = fileEntry.relativePath;
      li.dataset.relativePath = fileEntry.relativePath;
    }
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
  const entries = Array.from(fileList)
    .map(item =>
      item && typeof item === 'object' && 'file' in item
        ? createFileEntry(item.file, item.relativePath)
        : createFileEntry(item)
    )
    .filter(Boolean);
  const existing = new Set(queuedIngestFiles.map(getFileQueueKey).filter(Boolean));
  entries.forEach(entry => {
    const key = getFileQueueKey(entry);
    if (key && !existing.has(key)) {
      queuedIngestFiles.push(entry);
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
  const totalFiles = queuedIngestFiles.length;
  updateUploadProgress(0, totalFiles);
  if (ingestUploadStatusEl) {
    ingestUploadStatusEl.textContent = `Uploading ${totalFiles} file${totalFiles === 1 ? '' : 's'}…`;
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

  let uploadError = null;
  let lastIngestResponse = null;
  const formData = new FormData();
  queuedIngestFiles.forEach(fileEntry => {
    const { file, relativePath } = fileEntry;
    const uploadName = relativePath || getFileDisplayName(fileEntry) || file.name;
    formData.append('files', file, uploadName || file.name);
  });
  if (flowConfig.knowledge.collection) {
    formData.append('collection', flowConfig.knowledge.collection);
  }
  if (trimmedRepoPath) {
    formData.append('repo', flowConfig.knowledge.repo);
  }
  if (activeProjectId) {
    formData.append('project_id', activeProjectId);
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
    uploadError = err;
    if (ingestUploadStatusEl) {
      ingestUploadStatusEl.textContent = `Upload failed: ${err.message}`;
    }
  } finally {
    uploadInFlight = false;
    setUploadControlsDisabled(false);
  }
  if (uploadError) {
    resetUploadProgress();
    return;
  }
  updateUploadProgress(totalFiles, totalFiles);
  if (ingestUploadStatusEl) {
    let statusText = `Upload complete. ${totalFiles} file${totalFiles === 1 ? '' : 's'} uploaded successfully.`;
    if (lastIngestResponse && typeof lastIngestResponse.documents === 'number') {
      statusText += ` Indexed ${lastIngestResponse.documents} documents.`;
    }
    if (lastIngestResponse && lastIngestResponse.warning) {
      statusText += ` Warning: ${lastIngestResponse.warning}`;
    }
    ingestUploadStatusEl.textContent = statusText;
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
  resetUploadProgress();
  markStepComplete(2);
  activatePanel('docs', { allowLocked: true });
}
