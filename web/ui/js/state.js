const knowledgeFlows = new Set(['knowledge-base', 'chat-with-code', 'doc-generation', 'code-conversion']);
const modernizationFlows = new Set(['code-conversion']);
const angularMigrationFlows = new Set([
  'component-alignment',
  'state-management',
  'routing-strategy'
]);

const migrationConfig = {
  source: {
    framework: 'Angular',
    version: '17',
    patterns: {
      stateManagement: 'NgRx',
      componentArchitecture: 'Standalone Components'
    }
  },
  target: {
    framework: 'React',
    version: '18',
    patterns: {
      stateManagement: 'Redux Toolkit',
      componentArchitecture: 'Functional Components with Hooks'
    }
  },
  preferences: {
    routing: 'React Router',
    styling: 'CSS Modules',
    patterns: ['smart-container', 'presentational-components']
  }
};

const flowConfig = {
  knowledge: {
    repo: '',
    stacks: [],
    collection: ''
  },
  modernization: {
    language: '',
    version: '',
    framework: '',
    runtime: '',
    notes: ''
  },
  migration: migrationConfig
};

let selectedFlow = null;
let projectIdentifier = '';

const flowListeners = new Set();
const projectListeners = new Set();

export function getSelectedFlow() {
  return selectedFlow;
}

export function setSelectedFlow(flow) {
  const normalized = typeof flow === 'string' && flow.trim() ? flow.trim() : null;
  if (selectedFlow === normalized) {
    return selectedFlow;
  }
  selectedFlow = normalized;
  flowListeners.forEach(listener => {
    try {
      listener(selectedFlow);
    } catch (err) {
      console.error('Flow listener failed', err);
    }
  });
  return selectedFlow;
}

export function onFlowChange(listener) {
  if (typeof listener !== 'function') {
    return () => {};
  }
  flowListeners.add(listener);
  return () => flowListeners.delete(listener);
}

export function getKnowledgeFlows() {
  return knowledgeFlows;
}

export function getModernizationFlows() {
  return modernizationFlows;
}

export function getAngularMigrationFlows() {
  return angularMigrationFlows;
}

export function getFlowConfig() {
  return flowConfig;
}

export function getMigrationConfig() {
  return migrationConfig;
}

export function setProjectIdentifier(value) {
  const trimmed = typeof value === 'string' ? value.trim() : '';
  if (projectIdentifier === trimmed) {
    return projectIdentifier;
  }
  projectIdentifier = trimmed;
  projectListeners.forEach(listener => {
    try {
      listener(projectIdentifier);
    } catch (err) {
      console.error('Project listener failed', err);
    }
  });
  return projectIdentifier;
}

export function getProjectIdentifier(preferred = '') {
  const trimmed = typeof preferred === 'string' ? preferred.trim() : '';
  if (trimmed) {
    return trimmed;
  }
  return projectIdentifier;
}

export function onProjectIdentifierChange(listener) {
  if (typeof listener !== 'function') {
    return () => {};
  }
  projectListeners.add(listener);
  return () => projectListeners.delete(listener);
}
