// Compatibility shim for thread-stream on @types/node@26.
// The package still references the removed worker_threads.TransferListItem alias.
import type { Transferable } from "node:worker_threads";

declare module "worker_threads" {
  export type TransferListItem = Transferable;
}
