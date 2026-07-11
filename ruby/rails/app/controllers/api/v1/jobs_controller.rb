module Api
  module V1
    class JobsController < BaseController
      # POST /api/v1/jobs/process_data
      def process_data
        payload = params[:data] || {}
        job = BackgroundJobService.enqueue_job(:process_data, payload)
        
        if job
          render json: {
            message: 'Job enqueued successfully',
            job_type: 'process_data',
            job_id: SecureRandom.uuid,
            payload: payload,
            status: 'queued'
          }, status: :accepted
        else
          render json: { error: 'Failed to enqueue job' }, status: :service_unavailable
        end
      end
      
      # POST /api/v1/jobs/send_notification
      def send_notification
        payload = params[:notification] || {}
        job = BackgroundJobService.enqueue_job(:send_notification, payload)
        
        if job
          render json: {
            message: 'Notification job enqueued successfully',
            job_type: 'send_notification',
            job_id: SecureRandom.uuid,
            payload: payload,
            status: 'queued'
          }, status: :accepted
        else
          render json: { error: 'Failed to enqueue notification job' }, status: :service_unavailable
        end
      end
      
      # POST /api/v1/jobs/sync_external
      def sync_external
        payload = params[:data] || {}
        job = BackgroundJobService.enqueue_job(:sync_external, payload)
        
        if job
          render json: {
            message: 'Sync job enqueued successfully',
            job_type: 'sync_external',
            job_id: SecureRandom.uuid,
            payload: payload,
            status: 'queued'
          }, status: :accepted
        else
          render json: { error: 'Failed to enqueue sync job' }, status: :service_unavailable
        end
      end
      
      # GET /api/v1/jobs/:id/status
      def status
        job_id = params[:id]
        job_status = BackgroundJobService.job_status(job_id)
        
        render json: job_status
      end
      
      # GET /api/v1/jobs/stats
      def stats
        # In a real implementation, this would query the job queue stats
        stats = {
          total_jobs: rand(100..500),
          completed_jobs: rand(50..400),
          failed_jobs: rand(0..10),
          queued_jobs: rand(10..100),
          workers: rand(1..10),
          timestamp: Time.current.iso8601
        }
        
        render json: stats
      end
      
      # POST /api/v1/jobs/batch
      def batch
        jobs = params[:jobs] || []
        results = []
        
        jobs.each do |job_params|
          job_type = job_params[:type]
          payload = job_params[:payload] || {}
          
          job = BackgroundJobService.enqueue_job(job_type.to_sym, payload)
          
          if job
            results << {
              job_type: job_type,
              job_id: SecureRandom.uuid,
              payload: payload,
              status: 'queued'
            }
          end
        end
        
        if results.any?
          render json: {
            message: 'Batch jobs enqueued successfully',
            jobs: results,
            total: results.length
          }, status: :accepted
        else
          render json: { error: 'Failed to enqueue batch jobs' }, status: :service_unavailable
        end
      end
    end
  end
end
