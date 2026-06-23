#' @keywords internal
validate_scores <- function(scores) {
  if (!is.data.frame(scores))
    stop("`scores` must be a data frame")
  required <- c("id", "pop", "PC1", "PC2")
  missing_cols <- setdiff(required, colnames(scores))
  if (length(missing_cols) > 0)
    stop("`scores` is missing column(s): ",
         paste(missing_cols, collapse = ", "))
  dup_ids <- scores$id[duplicated(scores$id)]
  if (length(dup_ids) > 0)
    warning("Duplicate sample IDs detected. IDs: ",
            paste(dup_ids, collapse = ", "))
  invisible(TRUE)
}

#' @keywords internal
gadget_css <- function() {
  '
  body { font-family: Arial, sans-serif; }
  .controls { padding: 8px 0; }
  .controls .btn { margin-right: 4px; }
  .info-row { display: flex; gap: 20px; padding: 6px 0;
              font-size: 13px; color: #555; }
  .selected-box { background:#fff5f5; border:1px solid #fcc;
                  border-radius:4px; padding:8px; margin-top:8px;
                  font-family:monospace; font-size:11px;
                  white-space:pre-wrap; word-break:break-all;
                  max-height: 180px; overflow-y: auto; }
  .axis-controls { display:flex; gap:8px; align-items:center;
                   padding:6px 0; font-size:13px; flex-wrap:wrap; }
  .axis-controls select { padding:3px 6px; }
  .axis-controls .shiny-input-container { margin-bottom:0 !important; }
  .axis-controls .irs { margin:0 !important; }
  .axis-controls .irs-with-grid { height: 30px; }
  '
}
