# Manual QA

## Workflow auto-selection
- Start a workflow and leave it running.
- Reload the UI while the workflow continues running.
- Confirm that the running workflow card and the progress pane populate automatically without entering a project identifier.

## File upload directory preservation
- Prepare an Angular project directory with nested folders (e.g., `src/app`, `src/assets`).
- Drag and drop the entire directory onto the ingest drop zone.
- Verify that the queued file list displays relative paths (e.g., `src/app/app.module.ts`).
- Complete an upload and confirm on the server that the directory hierarchy is preserved for the ingested files.
