export async function postJSON(url, payload) {
  const res = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload || {})
  });
  const data = await res.json().catch(() => ({ error: 'Invalid JSON response' }));
  if (!res.ok) {
    throw new Error(data.error || res.statusText);
  }
  return data;
}

export async function getJSON(url) {
  const res = await fetch(url);
  const data = await res.json().catch(() => ({ error: 'Invalid JSON response' }));
  if (!res.ok) {
    throw new Error(data.error || res.statusText);
  }
  return data;
}

export function escapeHtml(str) {
  if (str == null) {
    return '';
  }
  const map = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' };
  return String(str).replace(/[&<>"']/g, ch => map[ch] || ch);
}

export function formatMultiline(text) {
  return escapeHtml(text).replace(/\n/g, '<br/>');
}

export function formatTimestamp(value) {
  if (!value) {
    return '';
  }
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return escapeHtml(value);
  }
  return date.toLocaleString();
}

export function formatFileSize(bytes) {
  if (!Number.isFinite(bytes)) {
    return '';
  }
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let size = bytes;
  let unitIndex = 0;
  while (size >= 1024 && unitIndex < units.length - 1) {
    size /= 1024;
    unitIndex += 1;
  }
  const precision = size >= 10 || unitIndex === 0 ? 0 : 1;
  return `${size.toFixed(precision)} ${units[unitIndex]}`;
}

export function populateDatalist(datalistEl, values) {
  if (!datalistEl) {
    return;
  }
  const seen = new Set();
  const options = Array.isArray(values)
    ? values.filter(value => {
        if (typeof value !== 'string') {
          return false;
        }
        const trimmed = value.trim();
        if (!trimmed || seen.has(trimmed)) {
          return false;
        }
        seen.add(trimmed);
        return true;
      })
    : [];
  datalistEl.innerHTML = options
    .map(value => `<option value="${escapeHtml(value)}"></option>`)
    .join('');
}
