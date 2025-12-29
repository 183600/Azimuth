// telemetry/sdk/trace/processor.mbt

// SimpleSpanProcessor exports spans synchronously as they end
pub type SimpleSpanProcessor {
  exporter : SpanExporter
}

pub fn SimpleSpanProcessor::new(exporter : SpanExporter) -> SimpleSpanProcessor {
  { exporter: exporter }
}

impl SpanProcessor for SimpleSpanProcessor {
  fn on_start(self, span : ReadWriteSpan, parent_context? : Option[@context.Context]) -> Unit {
    // Do nothing for simple processor
  }
  
  fn on_end(self, span : ReadableSpan) -> Unit {
    // Export synchronously
    // Note: In a real implementation, this should handle errors gracefully
    async {
      self.exporter.export([span]) |> ignore
    }()
  }
  
  async fn force_flush(self) -> Unit {
    self.exporter.force_flush() |> ignore
  }
  
  async fn shutdown(self) -> Unit {
    self.exporter.shutdown() |> ignore
  }
}

// BatchSpanProcessor exports spans in batches
pub type BatchSpanProcessor {
  exporter : SpanExporter
  max_queue_size : Int
  scheduled_delay_millis : Int
  max_export_batch_size : Int
  export_timeout_millis : Int
  // In a real implementation, this would have:
  // - A queue for spans
  // - A background worker for batch exports
  // - Timer for scheduled exports
  // - Mutex for thread safety
}

pub fn BatchSpanProcessor::builder(exporter : SpanExporter) -> BatchSpanProcessorBuilder {
  BatchSpanProcessorBuilder::new(exporter)
}

// Builder for BatchSpanProcessor
pub type BatchSpanProcessorBuilder {
  exporter : SpanExporter
  max_queue_size : Int
  scheduled_delay_millis : Int
  max_export_batch_size : Int
  export_timeout_millis : Int
}

pub fn BatchSpanProcessorBuilder::new(exporter : SpanExporter) -> BatchSpanProcessorBuilder {
  {
    exporter: exporter,
    max_queue_size: 2048,
    scheduled_delay_millis: 5000,
    max_export_batch_size: 512,
    export_timeout_millis: 30000
  }
}

impl BatchSpanProcessorBuilder {
  pub fn with_max_queue_size(self, size : Int) -> BatchSpanProcessorBuilder {
    { self | max_queue_size: size }
  }
  
  pub fn with_scheduled_delay(self, delay_millis : Int) -> BatchSpanProcessorBuilder {
    { self | scheduled_delay_millis: delay_millis }
  }
  
  pub fn with_max_export_batch_size(self, size : Int) -> BatchSpanProcessorBuilder {
    { self | max_export_batch_size: size }
  }
  
  pub fn with_export_timeout(self, timeout_millis : Int) -> BatchSpanProcessorBuilder {
    { self | export_timeout_millis: timeout_millis }
  }
  
  pub fn build(self) -> BatchSpanProcessor {
    {
      exporter: self.exporter,
      max_queue_size: self.max_queue_size,
      scheduled_delay_millis: self.scheduled_delay_millis,
      max_export_batch_size: self.max_export_batch_size,
      export_timeout_millis: self.export_timeout_millis
    }
  }
}

impl SpanProcessor for BatchSpanProcessor {
  fn on_start(self, span : ReadWriteSpan, parent_context? : Option[@context.Context]) -> Unit {
    // In a real implementation, this would add the span to a queue
  }
  
  fn on_end(self, span : ReadableSpan) -> Unit {
    // In a real implementation, this would add the span to the export queue
    // and let the background worker handle batching
    async {
      self.exporter.export([span]) |> ignore
    }()
  }
  
  async fn force_flush(self) -> Unit {
    // In a real implementation, this would flush the queue
    self.exporter.force_flush() |> ignore
  }
  
  async fn shutdown(self) -> Unit {
    // In a real implementation, this would flush remaining spans and shutdown the worker
    self.exporter.shutdown() |> ignore
  }
}

// ReadWriteSpan interface for spans that can be modified
pub trait ReadWriteSpan {
  fn span_context(self) -> SpanContext
  fn is_recording(self) -> Bool
  fn set_attribute(self, key : String, value : @common.AttributeValue) -> Unit
  fn add_event(self, name : String, attributes? : @common.Attributes, timestamp? : Int64) -> Unit
  fn set_status(self, code : StatusCode, description? : String) -> Unit
}