import { renderMarkdown, maybeRenderMermaid } from './markdown.js';
import { formatTimestamp } from './utils.js';
import { getSelectedFlow, onFlowChange } from './state.js';

let consolidatedPane;
let consolidatedStatusEl;
let consolidatedViewerEl;
let consolidatedContentEl;
let consolidatedRefreshBtn;
let consolidatedDownloadBtn;
let consolidatedCopyBtn;
let currentProjectId = '';
let loadedProjectId = '';
let loadedSignature = '';
let loadedMarkdown = '';
let fetchController = null;
let flowUnsubscribe = null;

export function initConsolidatedDoc() {
  consolidatedPane = document.getElementById('consolidatedDocPane');
  consolidatedStatusEl = document.getElementById('consolidatedDocStatus');
  consolidatedViewerEl = document.getElementById('consolidatedDocViewer');
  consolidatedContentEl = document.getElementById('consolidatedDocContent');
  consolidatedRefreshBtn = document.getElementById('consolidatedDocView');
  consolidatedDownloadBtn = document.getElementById('consolidatedDocDownload');
  consolidatedCopyBtn = document.getElementById('consolidatedDocCopy');

  if (consolidatedPane && !consolidatedPane.dataset.hasArtifacts) {
    consolidatedPane.dataset.hasArtifacts = 'false';
  }

  if (consolidatedRefreshBtn) {
    consolidatedRefreshBtn.addEventListener('click', () => {
      if (!currentProjectId) {
        announceStatus('Set a project identifier to load consolidated documentation.', true);
        return;
      }
      checkConsolidatedDoc(currentProjectId, { force: true });
    });
  }

  if (consolidatedDownloadBtn) {
    consolidatedDownloadBtn.addEventListener('click', () => {
      if (!currentProjectId) {
        announceStatus('Select a project before downloading the consolidated package.', true);
        return;
      }
      const url = `/api/consolidated-doc?project_id=${encodeURIComponent(currentProjectId)}&download=1`;
      window.open(url, '_blank');
    });
  }

  if (consolidatedCopyBtn) {
    consolidatedCopyBtn.addEventListener('click', async () => {
      if (!loadedMarkdown) {
        announceStatus('Consolidated markdown is not available yet.', true);
        return;
      }
      try {
        await navigator.clipboard.writeText(loadedMarkdown);
        announceStatus('Copied consolidated markdown to the clipboard.');
      } catch (err) {
        console.error('clipboard copy failed', err);
        announceStatus('Failed to copy consolidated markdown. Try again or copy manually.', true);
      }
    });
  }

  if (!flowUnsubscribe && typeof onFlowChange === 'function') {
    flowUnsubscribe = onFlowChange(() => updatePaneVisibility());
  }

  resetConsolidatedDoc('Consolidated documentation will appear here after a workflow completes.');
}

export async function checkConsolidatedDoc(projectId, options = {}) {
  if (!consolidatedPane) {
    return;
  }
  const normalized = normalizeProjectId(projectId);
  currentProjectId = normalized;
  const force = Boolean(options && options.force);

  if (!normalized) {
    loadedProjectId = '';
    loadedSignature = '';
    loadedMarkdown = '';
    updateButtonState(false);
    consolidatedPane.dataset.hasArtifacts = 'false';
    resetConsolidatedDoc('Consolidated documentation will appear here after a workflow completes.');
    return;
  }

  if (fetchController) {
    fetchController.abort();
  }
  fetchController = new AbortController();
  announceStatus('Checking for consolidated documentation…');
  setLoading(true);

  try {
    const response = await fetch(`/api/consolidated-doc?project_id=${encodeURIComponent(normalized)}`, {
      signal: fetchController.signal
    });
    if (response.status === 404) {
      handleMissingConsolidatedDoc();
      return;
    }

    let payload;
    try {
      payload = await response.json();
    } catch (parseErr) {
      throw new Error('Invalid response when loading consolidated documentation.');
    }

    if (!response.ok) {
      const message = payload && payload.error ? payload.error : response.statusText || 'Request failed';
      throw new Error(message);
    }

    const manifest = payload && payload.manifest ? payload.manifest : null;
    const artifact = payload && payload.artifact ? payload.artifact : null;
    const markdown = buildConsolidatedMarkdown(manifest);
    const signature = buildManifestSignature(manifest);

    if (!markdown) {
      consolidatedPane.dataset.hasArtifacts = 'false';
      loadedProjectId = normalized;
      loadedSignature = signature;
      loadedMarkdown = '';
      resetConsolidatedDoc('Consolidated documentation is not available yet.');
      return;
    }

    const alreadyCurrent =
      !force && normalized === loadedProjectId && signature && signature === loadedSignature && Boolean(loadedMarkdown);
    loadedProjectId = normalized;
    loadedMarkdown = markdown;
    loadedSignature = signature;
    consolidatedPane.dataset.hasArtifacts = 'true';
    updatePaneVisibility();
    renderConsolidatedMarkdown(markdown, manifest, artifact, alreadyCurrent);
    updateButtonState(true);
  } catch (err) {
    if (err.name === 'AbortError') {
      return;
    }
    console.error('failed to load consolidated documentation', err);
    announceStatus(err && err.message ? err.message : 'Failed to load consolidated documentation.', true);
    updateButtonState(Boolean(loadedMarkdown));
  } finally {
    setLoading(false);
    fetchController = null;
  }
}

function normalizeProjectId(projectId) {
  return projectId == null ? '' : String(projectId).trim();
}

function setLoading(isLoading) {
  if (!consolidatedPane) {
    return;
  }
  if (isLoading) {
    consolidatedPane.classList.add('loading');
  } else {
    consolidatedPane.classList.remove('loading');
  }
  if (consolidatedRefreshBtn) {
    consolidatedRefreshBtn.disabled = isLoading || !currentProjectId;
  }
}

function announceStatus(message, isWarning = false) {
  if (!consolidatedStatusEl) {
    return;
  }
  consolidatedStatusEl.textContent = message || '';
  if (isWarning) {
    consolidatedStatusEl.classList.add('warning');
  } else {
    consolidatedStatusEl.classList.remove('warning');
  }
}

function resetConsolidatedDoc(message) {
  if (consolidatedContentEl) {
    consolidatedContentEl.innerHTML = '';
    consolidatedContentEl.hidden = true;
  }
  if (consolidatedViewerEl) {
    consolidatedViewerEl.dataset.hasContent = 'false';
  }
  updateButtonState(false);
  announceStatus(message || 'Consolidated documentation is not available yet.');
  setLoading(false);
  updatePaneVisibility();
}

function handleMissingConsolidatedDoc() {
  consolidatedPane.dataset.hasArtifacts = 'false';
  loadedProjectId = currentProjectId;
  loadedSignature = '';
  loadedMarkdown = '';
  resetConsolidatedDoc('Consolidated documentation is not available yet.');
}

function updateButtonState(hasContent) {
  const disabled = !hasContent;
  if (consolidatedDownloadBtn) {
    consolidatedDownloadBtn.disabled = !currentProjectId || disabled;
  }
  if (consolidatedCopyBtn) {
    consolidatedCopyBtn.disabled = disabled;
  }
}

function updatePaneVisibility() {
  if (!consolidatedPane) {
    return;
  }
  const flows = (consolidatedPane.dataset.flows || '')
    .split(',')
    .map(value => value.trim())
    .filter(Boolean);
  const hasArtifacts = consolidatedPane.dataset.hasArtifacts === 'true';
  let flowMatches = true;
  if (flows.length) {
    const currentFlow = getSelectedFlow();
    flowMatches = Boolean(currentFlow && flows.includes(currentFlow));
  }
  const shouldShow = hasArtifacts || flowMatches;
  consolidatedPane.hidden = !shouldShow;
  if (shouldShow) {
    consolidatedPane.removeAttribute('aria-hidden');
  } else {
    consolidatedPane.setAttribute('aria-hidden', 'true');
  }
}

function renderConsolidatedMarkdown(markdown, manifest, artifact, alreadyCurrent = false) {
  if (!consolidatedContentEl) {
    return;
  }
  const html = renderMarkdown(markdown);
  consolidatedContentEl.innerHTML = html;
  consolidatedContentEl.hidden = false;
  if (consolidatedViewerEl) {
    consolidatedViewerEl.dataset.hasContent = 'true';
  }
  maybeRenderMermaid(consolidatedContentEl);
  const summaryParts = buildSummary(manifest, artifact);
  if (summaryParts.length) {
    const summary = summaryParts.join(' • ');
    if (alreadyCurrent) {
      announceStatus(`Consolidated documentation is up to date • ${summary}`);
    } else {
      announceStatus(summary);
    }
  } else if (alreadyCurrent) {
    announceStatus('Consolidated documentation is already up to date.');
  } else {
    announceStatus('Consolidated documentation refreshed.');
  }
}

function buildSummary(manifest, artifact) {
  const parts = [];
  if (manifest && typeof manifest.document_count === 'number' && manifest.document_count > 0) {
    const label = manifest.document_count === 1 ? 'document' : 'documents';
    parts.push(`${manifest.document_count} ${label}`);
  }
  if (manifest && manifest.generated_at) {
    const formatted = formatTimestamp(manifest.generated_at);
    if (formatted) {
      parts.push(`Generated ${formatted}`);
    }
  }
  if (artifact && artifact.name) {
    parts.push(`Artifact: ${String(artifact.name)}`);
  }
  if (manifest && manifest.project_id) {
    parts.push(`Project: ${manifest.project_id}`);
  }
  return parts;
}

function buildConsolidatedMarkdown(manifest) {
  if (!manifest || typeof manifest !== 'object') {
    return '';
  }
  const documentCount = Number(manifest.document_count || 0);
  if (!documentCount) {
    return '';
  }
  const lines = ['# Project Documentation Summary', ''];
  if (manifest.project_id) {
    lines.push(`Project: ${manifest.project_id}`);
    lines.push('');
  }
  if (manifest.generated_at) {
    lines.push(`Generated: ${manifest.generated_at}`);
    lines.push('');
  }
  const programs = Array.isArray(manifest.programs) ? manifest.programs : [];
  programs.forEach(program => {
    const programName = program && program.program ? String(program.program).trim() : '';
    if (programName) {
      lines.push(`## ${programName}`);
      lines.push('');
    }
    const docs = program && Array.isArray(program.documents) ? program.documents : [];
    docs.forEach(doc => {
      const title = pickDocTitle(doc);
      if (title) {
        lines.push(`### ${title}`);
        lines.push('');
      }
      const type = doc && doc.type ? String(doc.type).trim() : '';
      if (type) {
        lines.push(`_Type_: ${type}`);
        lines.push('');
      }
      const source = doc && doc.source ? String(doc.source).trim() : '';
      if (source) {
        lines.push(`_Source_: ${source}`);
        lines.push('');
      }
      const summary = doc && doc.summary ? String(doc.summary).trim() : '';
      if (summary && summary !== title) {
        lines.push(`_Summary_: ${summary}`);
        lines.push('');
      }
      const markdown = doc && doc.markdown ? String(doc.markdown) : '';
      if (markdown) {
        lines.push(markdown.trimEnd());
        lines.push('');
      }
    });
  });
  return lines.join('\n').trim() + '\n';
}

function pickDocTitle(doc) {
  const title = doc && doc.title ? String(doc.title).trim() : '';
  if (title) {
    return title;
  }
  const type = doc && doc.type ? String(doc.type).trim() : '';
  if (type) {
    return type;
  }
  const id = doc && doc.id ? String(doc.id).trim() : '';
  return id;
}

function buildManifestSignature(manifest) {
  if (!manifest || typeof manifest !== 'object') {
    return '';
  }
  const parts = [manifest.project_id || '', manifest.generated_at || '', String(manifest.document_count || '')];
  if (Array.isArray(manifest.documents)) {
    parts.push(String(manifest.documents.length));
  }
  return parts.join('|');
}
