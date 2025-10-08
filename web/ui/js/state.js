const knowledgeFlows = new Set(['knowledge-base', 'chat-with-code', 'doc-generation', 'code-conversion']);
const modernizationFlows = new Set(['code-conversion', 'angular-modernization']);
const angularMigrationFlows = new Set(['angular-react-migration']);

const targetPresetFactories = new Map([
  ['code-conversion', () => flowConfig.modernization],
  ['angular-modernization', () => flowConfig.modernization],
  ['angular-react-migration', () => migrationConfig.target]
]);

function clonePreset(config) {
  if (!config || typeof config !== 'object') {
    return null;
  }
  const clone = Array.isArray(config) ? config.slice() : { ...config };
  Object.keys(clone).forEach(key => {
    const value = clone[key];
    if (Array.isArray(value)) {
      clone[key] = value.slice();
    } else if (value && typeof value === 'object') {
      clone[key] = clonePreset(value);
    }
  });
  if (clone.framework != null) {
    clone.framework = String(clone.framework).trim().toLowerCase();
  }
  if (clone.version != null) {
    clone.version = String(clone.version).trim();
  }
  return clone;
}

const migrationConfig = {
  source: {
    framework: 'Angular',
    version: 'auto',
    detectedVersion: '',
    moduleType: 'standalone',
    buildSystem: 'angular-cli',
    sourceRoot: './src/app'
  },
  target: {
    framework: 'React',
    version: '18',
    componentPattern: 'functional-hooks',
    stateManagement: 'redux-toolkit',
    routing: 'react-router',
    uiLibraries: ['mui'],
    targetRoot: './src/react-app'
  },
  mappingText: {
    componentLifecycle: [
      'OnInit => useEffect(() => initialize(), [])',
      'OnDestroy => useEffect(() => () => cleanup(), [])',
      'AfterViewInit => useLayoutEffect(() => hydrateView(), [])',
      'AfterViewChecked => useEffect(() => syncViewWithState(), [dependencies])',
      'DoCheck => useEffect(() => runCustomChangeDetection(), [dependencies])'
    ].join('\n'),
    directiveConversions: [
      '*ngIf => withConditionalRender(condition, Component)',
      '*ngFor => useListRenderer(items, renderItem)',
      '[(ngModel)] => useControlledField(binding)',
      '[ngClass] => withDynamicClasses(classMap)',
      '[ngStyle] => withInlineStyles(styleMap)'
    ].join('\n'),
    serviceContexts: [
      'AuthService => useAuthContext()',
      'UserService => useUserContext()',
      'HttpClient => useHttpClient()',
      'Store => useReduxStore()',
      'Router => useRouter()'
    ].join('\n'),
    pipeConversions: [
      'date => formatDate(value, locale)',
      'currency => formatCurrency(value, currencyCode)',
      'async => useAsyncValue(observable)',
      'json => JSON.stringify(value, null, 2)',
      'uppercase => value.toUpperCase()'
    ].join('\n'),
    guardRoutes: [
      'AuthGuard => requireAuth(Component)',
      'RoleGuard => requireRole(Component, roles)',
      'CanActivateChild => protectChildRoutes(children)',
      'CanLoad => lazyLoadWithGuard(factory)',
      'CanActivate => guardRoute(Component, condition)'
    ].join('\n')
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

export function getTargetPresetConfig(flow) {
  if (!flow) {
    return null;
  }
  const factory = targetPresetFactories.get(flow);
  if (!factory) {
    return null;
  }
  const config = factory();
  if (!config || typeof config !== 'object') {
    return null;
  }
  return clonePreset(config);
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
