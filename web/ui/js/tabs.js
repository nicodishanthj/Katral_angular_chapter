const tabs = Array.from(document.querySelectorAll('.tab'));
const panels = Array.from(document.querySelectorAll('.panel'));

const initialStep = Number(document.querySelector('.tab.active')?.dataset.step || '1');
const stepState = {
  current: Number.isFinite(initialStep) ? initialStep : 1,
  highestUnlocked: Number.isFinite(initialStep) ? initialStep : 1,
  completed: new Set()
};

const autoCompleteSteps = new Set([3]);

function updateTabStates() {
  tabs.forEach(tab => {
    const step = Number(tab.dataset.step || '0');
    const disabled = step > stepState.highestUnlocked;
    tab.disabled = disabled;
    tab.setAttribute('aria-disabled', disabled ? 'true' : 'false');
    tab.setAttribute('aria-current', step === stepState.current ? 'step' : 'false');
    tab.classList.toggle('disabled', disabled);
    tab.classList.toggle('completed', stepState.completed.has(step));
  });
}

function unlockThroughStep(step) {
  if (step > stepState.highestUnlocked) {
    stepState.highestUnlocked = step;
    updateTabStates();
  }
}

function completeArtifactsStep() {
  if (!stepState.completed.has(4)) {
    stepState.completed.add(4);
  }
  unlockThroughStep(5);
}

export function markStepComplete(step) {
  if (!step) {
    return;
  }
  const wasCompleted = stepState.completed.has(step);
  if (!wasCompleted) {
    stepState.completed.add(step);
  }
  if (step === 3) {
    completeArtifactsStep();
  } else if (!wasCompleted) {
    unlockThroughStep(step + 1);
  }
  updateTabStates();
}

export function activatePanel(panelId, options = {}) {
  const tabToActivate = tabs.find(tab => tab.dataset.panel === panelId);
  if (!tabToActivate) {
    return;
  }
  const step = Number(tabToActivate.dataset.step || '0');
  if (!options.allowLocked && step > stepState.highestUnlocked) {
    return;
  }
  tabs.forEach(tab => tab.classList.remove('active'));
  panels.forEach(panel => panel.classList.remove('active'));
  tabToActivate.classList.add('active');
  const panel = document.getElementById(panelId);
  if (panel) {
    panel.classList.add('active');
  }
  if (step) {
    stepState.current = step;
  }
  if (!options.skipCompletionUpdate && autoCompleteSteps.has(step)) {
    markStepComplete(step);
  } else {
    updateTabStates();
  }
}

export function initTabs() {
  tabs.forEach(tab => {
    tab.addEventListener('click', () => {
      const step = Number(tab.dataset.step || '0');
      if (step > stepState.highestUnlocked) {
        return;
      }
      activatePanel(tab.dataset.panel);
    });
  });
  updateTabStates();
}
