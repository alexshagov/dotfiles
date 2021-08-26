 ;; Remove GUI elements early to avoid some possible grapical glitches.
 (spacemacs/removes-gui-elements)
+
+;; Native-Comp
+(setq native-comp-speed 2
+      comp-speed 2)
+(setq native-comp-async-report-warnings-errors nil
+      comp-async-report-warnings-errors nil)
+(setq native-comp-async-query-on-exit t
+      comp-async-query-on-exit t)
\ No newline at end of file
diff --git a/layers/+spacemacs/spacemacs-defaults/config.el b/layers/+spacemacs/spacemacs-defaults/config.el
index 4f03165d3..81dd41665 100644
--- a/layers/+spacemacs/spacemacs-defaults/config.el
+++ b/layers/+spacemacs/spacemacs-defaults/config.el
@@ -238,4 +238,4 @@ It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
 (add-hook 'kill-buffer-hook #'spacemacs//add-buffer-to-killed-list)

 ;; Don't load outdated compiled files.
-(setq load-prefer-newer t)
+;; (setq load-prefer-newer t)
