import { getJSON, escapeHtml, formatMultiline, formatTimestamp } from './utils.js';

function formatLogValue(value) {
  if (value == null) {
    return 'null';
  }
  if (typeof value === 'object') {
    try {
      return JSON.stringify(value);
    } catch (err) {
      return String(value);
    }
  }
  return String(value);
}

function formatLogAttributes(attrs) {
  if (!attrs || typeof attrs !== 'object') {
    return '';
  }
  const pairs = Object.entries(attrs).filter(([key]) => key);
  if (!pairs.length) {
    return '';
  }
  return pairs
    .map(([key, value]) => `<code>${escapeHtml(key)}</code>=${escapeHtml(formatLogValue(value))}`)
    .join(' · ');
}

export function initLogs() {
  const logsResultEl = document.getElementById('logsResult');
  if (!logsResultEl) {
    return;
  }
  const logsRefreshBtn = document.getElementById('logsRefresh');
  let logsPollId = null;

  const fetchLogs = async (manual = false) => {
    try {
      const data = await getJSON('/v1/logs');
      if (!data.entries || !data.entries.length) {
        logsResultEl.textContent = 'No log entries yet.';
        return;
      }
      logsResultEl.innerHTML = data.entries
        .map(entry => {
          const timestamp = formatTimestamp(entry.time);
          const levelKey = entry.level ? String(entry.level).toLowerCase() : '';
          const safeLevelKey = levelKey.replace(/[^a-z0-9]+/g, '-');
          const levelLabel = levelKey ? levelKey.toUpperCase() : '';
          const message = formatMultiline(entry.message || '');
          const component = entry.component
            ? `<span class="log-component">${escapeHtml(entry.component)}</span>`
            : '';
          const levelBadge = levelLabel
            ? `<span class="log-level${safeLevelKey ? ' log-level-' + safeLevelKey : ''}">${escapeHtml(levelLabel)}</span>`
            : '';
          const attributes = formatLogAttributes(entry.attributes);
          const attributesHtml = attributes ? `<div class="log-attributes">${attributes}</div>` : '';
          return `
            <div class="log-entry">
              <div class="log-meta">
                <span class="log-time">${escapeHtml(timestamp)}</span>
                ${component}
                ${levelBadge}
              </div>
              <div class="log-message">${message}</div>
              ${attributesHtml}
            </div>
          `;
        })
        .join('');
      logsResultEl.scrollTop = logsResultEl.scrollHeight;
    } catch (err) {
      if (manual) {
        logsResultEl.textContent = 'Error: ' + escapeHtml(err.message || String(err));
      }
    }
  };

  if (logsRefreshBtn) {
    logsRefreshBtn.addEventListener('click', () => fetchLogs(true));
  }

  logsPollId = setInterval(fetchLogs, 5000);
  fetchLogs();

  return () => {
    if (logsPollId) {
      clearInterval(logsPollId);
    }
  };
}
