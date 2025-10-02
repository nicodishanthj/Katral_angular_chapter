import { escapeHtml } from './utils.js';

let mermaidInitialized = false;
let mermaidRequested = false;

export function renderFlowDiagramBlock(diagram) {
  const mermaidSource = flowDiagramToMermaid(diagram);
  if (!mermaidSource) {
    return '';
  }
  mermaidRequested = true;
  return `<div class="flow-diagram"><pre class="mermaid">${escapeHtml(mermaidSource)}</pre></div>`;
}

function flowDiagramToMermaid(diagram) {
  if (!diagram || !Array.isArray(diagram.nodes) || !diagram.nodes.length) {
    return '';
  }
  const idMap = new Map();
  const lines = ['flowchart TD'];
  diagram.nodes.forEach(node => {
    if (!node || !node.id || idMap.has(node.id)) {
      return;
    }
    const sanitizedId = sanitizeMermaidId(node.id);
    idMap.set(node.id, sanitizedId);
    const label = escapeMermaidText(node.label || node.type || node.id);
    lines.push(`${sanitizedId}["${label}"]`);
  });
  if (lines.length === 1) {
    return '';
  }
  if (Array.isArray(diagram.edges)) {
    diagram.edges.forEach(edge => {
      if (!edge || !edge.from || !edge.to) {
        return;
      }
      const from = idMap.get(edge.from);
      const to = idMap.get(edge.to);
      if (!from || !to) {
        return;
      }
      const label = edge.label ? `|${escapeMermaidText(edge.label)}|` : '';
      lines.push(`${from} -->${label} ${to}`);
    });
  }
  return lines.length === 1 ? '' : lines.join('\n');
}

function sanitizeMermaidId(id) {
  return String(id == null ? '' : id).replace(/[^a-zA-Z0-9_]/g, '_');
}

function escapeMermaidText(value) {
  return String(value == null ? '' : value)
    .replace(/[\\"]/g, '\\$&')
    .replace(/\|/g, '/')
    .replace(/\n+/g, ' ')
    .trim();
}

function ensureMermaidInitialized() {
  if (mermaidInitialized) {
    return;
  }
  if (!window.mermaid || typeof window.mermaid.initialize !== 'function') {
    return;
  }
  try {
    window.mermaid.initialize({ startOnLoad: false, securityLevel: 'loose' });
    mermaidInitialized = true;
  } catch (err) {
    console.warn('Mermaid initialization failed', err);
  }
}

function renderMermaidDiagrams(container) {
  ensureMermaidInitialized();
  if (!mermaidInitialized || !window.mermaid || typeof window.mermaid.init !== 'function') {
    return;
  }
  const root = container || document;
  const targets = root.querySelectorAll('.mermaid');
  if (!targets.length) {
    return;
  }
  try {
    window.mermaid.init(undefined, targets);
  } catch (err) {
    console.warn('Mermaid rendering failed', err);
  }
}

export function maybeRenderMermaid(container) {
  if (!mermaidRequested) {
    return;
  }
  mermaidRequested = false;
  renderMermaidDiagrams(container);
}

export function renderMarkdown(markdown) {
  if (!markdown) {
    return '';
  }
  const rawHtml = markdownToHtml(markdown);
  return sanitizeHtml(rawHtml);
}

function markdownToHtml(markdown) {
  const normalized = String(markdown).replace(/\r\n?/g, '\n');
  const lines = normalized.split('\n');
  const parts = [];
  let inCodeBlock = false;
  let codeLang = '';
  let codeLines = [];
  let listType = null;
  let listBuffer = [];
  let paragraphLines = [];
  let blockquoteLines = null;

  const flushParagraph = () => {
    if (!paragraphLines.length) {
      return;
    }
    const text = paragraphLines.join('\n');
    const formatted = applyInlineFormatting(text).replace(/\n/g, '<br/>');
    parts.push(`<p>${formatted}</p>`);
    paragraphLines = [];
  };

  const flushList = () => {
    if (!listType || !listBuffer.length) {
      return;
    }
    const items = listBuffer.map(item => `<li>${applyInlineFormatting(item)}</li>`).join('');
    parts.push(`<${listType}>${items}</${listType}>`);
    listType = null;
    listBuffer = [];
  };

  const flushBlockquote = () => {
    if (!blockquoteLines || !blockquoteLines.length) {
      return;
    }
    const text = blockquoteLines.join('\n');
    const formatted = applyInlineFormatting(text).replace(/\n/g, '<br/>');
    parts.push(`<blockquote>${formatted}</blockquote>`);
    blockquoteLines = null;
  };

  const closeCodeBlock = () => {
    const codeContent = codeLines.join('\n');
    const langClass = codeLang ? ` class="language-${escapeHtml(codeLang)}"` : '';
    parts.push(`<pre><code${langClass}>${escapeHtml(codeContent)}</code></pre>`);
    inCodeBlock = false;
    codeLines = [];
    codeLang = '';
  };

  lines.forEach(line => {
    const fenceMatch = line.match(/^```(.*)$/);
    if (fenceMatch) {
      if (inCodeBlock) {
        closeCodeBlock();
      } else {
        flushParagraph();
        flushList();
        flushBlockquote();
        inCodeBlock = true;
        codeLang = fenceMatch[1].trim();
        codeLines = [];
      }
      return;
    }

    if (inCodeBlock) {
      codeLines.push(line);
      return;
    }

    const headingMatch = line.match(/^\s{0,3}(#{1,6})\s+(.*)$/);
    if (headingMatch) {
      flushParagraph();
      flushList();
      flushBlockquote();
      const level = Math.min(headingMatch[1].length, 6);
      const headingText = applyInlineFormatting(headingMatch[2].trim());
      parts.push(`<h${level}>${headingText}</h${level}>`);
      return;
    }

    if (!line.trim()) {
      flushParagraph();
      flushList();
      flushBlockquote();
      return;
    }

    if (/^\s*([-*_]){3,}\s*$/.test(line)) {
      flushParagraph();
      flushList();
      flushBlockquote();
      parts.push('<hr/>');
      return;
    }

    const blockquoteMatch = line.match(/^\s*>\s?(.*)$/);
    if (blockquoteMatch) {
      flushParagraph();
      flushList();
      if (!blockquoteLines) {
        blockquoteLines = [];
      }
      blockquoteLines.push(blockquoteMatch[1]);
      return;
    }

    const unorderedMatch = line.match(/^\s*[-*+]\s+(.*)$/);
    if (unorderedMatch) {
      flushParagraph();
      flushBlockquote();
      if (listType && listType !== 'ul') {
        flushList();
      }
      listType = 'ul';
      listBuffer.push(unorderedMatch[1]);
      return;
    }

    const orderedMatch = line.match(/^\s*(\d+)\.\s+(.*)$/);
    if (orderedMatch) {
      flushParagraph();
      flushBlockquote();
      if (listType && listType !== 'ol') {
        flushList();
      }
      listType = 'ol';
      listBuffer.push(orderedMatch[2]);
      return;
    }

    flushBlockquote();
    paragraphLines.push(line);
  });

  if (inCodeBlock) {
    closeCodeBlock();
  }
  flushParagraph();
  flushList();
  flushBlockquote();

  return parts.join('');
}

function applyInlineFormatting(text) {
  if (!text) {
    return '';
  }
  let result = escapeHtml(text);
  const codePlaceholders = [];
  result = result.replace(/`([^`]+)`/g, (match, code) => {
    const index = codePlaceholders.length;
    codePlaceholders.push(`<code>${code}</code>`);
    return `@@CODE${index}@@`;
  });
  result = result.replace(/~~([^~]+)~~/g, '<del>$1</del>');
  result = result.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
  result = result.replace(/\*([^*]+)\*/g, '<em>$1</em>');
  result = result.replace(/!\[([^\]]*)\]\(([^)]+)\)/g, (match, altText) => altText);
  result = result.replace(/\[([^\]]+)\]\(([^)]+)\)/g, (match, linkText, href) => {
    const safeHref = sanitizeUrl(href);
    if (!safeHref) {
      return linkText;
    }
    return `<a href="${safeHref}" target="_blank" rel="noopener noreferrer">${linkText}</a>`;
  });
  codePlaceholders.forEach((markup, index) => {
    result = result.replace(`@@CODE${index}@@`, markup);
  });
  return result;
}

function sanitizeUrl(url) {
  if (!url) {
    return '';
  }
  const trimmed = url.trim();
  if (!trimmed) {
    return '';
  }
  const lower = trimmed.toLowerCase();
  if (lower.startsWith('javascript:') || lower.startsWith('data:') || lower.startsWith('vbscript:')) {
    return '';
  }
  if (lower.startsWith('//')) {
    return '';
  }
  if (!(lower.startsWith('http://') || lower.startsWith('https://') || lower.startsWith('mailto:') || lower.startsWith('#') || lower.startsWith('/') || lower.startsWith('./') || lower.startsWith('../'))) {
    return '';
  }
  return escapeHtml(trimmed);
}

function sanitizeHtml(html) {
  const template = document.createElement('template');
  template.innerHTML = html;
  const allowedTags = new Set(['A', 'B', 'BR', 'BLOCKQUOTE', 'CODE', 'DEL', 'EM', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'HR', 'LI', 'OL', 'P', 'PRE', 'STRONG', 'UL']);
  const allowedAttributes = {
    A: new Set(['href', 'title', 'target', 'rel']),
    CODE: new Set(['class']),
    PRE: new Set(['class'])
  };
  const disallowedHref = /^(javascript|data|vbscript):/i;

  const elements = template.content.querySelectorAll('*');
  elements.forEach(node => {
    if (!allowedTags.has(node.tagName)) {
      node.replaceWith(...node.childNodes);
      return;
    }
    [...node.attributes].forEach(attr => {
      const attrName = attr.name.toLowerCase();
      const allowed = allowedAttributes[node.tagName] || new Set();
      if (!allowed.has(attrName)) {
        node.removeAttribute(attr.name);
        return;
      }
      if (node.tagName === 'A' && attrName === 'href') {
        const value = attr.value.trim();
        if (!value || disallowedHref.test(value)) {
          node.removeAttribute('href');
          node.removeAttribute('target');
          node.removeAttribute('rel');
          return;
        }
        if (!node.hasAttribute('target')) {
          node.setAttribute('target', '_blank');
        }
        node.setAttribute('rel', 'noopener noreferrer');
      }
      if ((node.tagName === 'CODE' || node.tagName === 'PRE') && attrName === 'class') {
        const safeClasses = attr.value
          .split(/\s+/)
          .filter(cls => cls && /^language-[\w-]+$/.test(cls));
        if (safeClasses.length) {
          node.setAttribute('class', safeClasses.join(' '));
        } else {
          node.removeAttribute('class');
        }
      }
    });
  });

  return template.innerHTML;
}
