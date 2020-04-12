import multiprocessing
import os
import sys

workers = multiprocessing.cpu_count()
bind = "0.0.0.0:3000"
worker_class = os.environ['WORKER_CLASS']

if worker_class == "meinheld.gmeinheld.MeinheldWorker":
    def post_fork(server, worker):
        # Disalbe access log
        import meinheld.server
        meinheld.server.set_access_logger(None)