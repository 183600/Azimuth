// telemetry/sdk/exporter/console/exporter.mbt

// ConsoleExporter exports spans to the console
pub type ConsoleSpanExporter {
  pretty_print : Bool
}

// Create a new console exporter
pub fn ConsoleSpanExporter::new(pretty_print? : Bool) -> ConsoleSpanExporter {
  {
    pretty_print: pretty_print.unwrap_or(true)
  }
}

impl SpanExporter for ConsoleSpanExporter {
  async fn export(self, spans : Array[ReadableSpan]) -> ExportResult {
    for span in spans {
      print_span(span, self.pretty_print)
    }
    Success
  }
  
  async fn force_flush(self) -> ExportResult {
    Success
  }
  
  async fn shutdown(self) -> ExportResult {
    Success
  }
}

// Print span to console
fn print_span(span : ReadableSpan, pretty_print : Bool) -> Unit {
  if pretty_print {
    println("=== Span ===")
    println("Name: ${span.name}")
    println("Trace ID: ${bytes_to_hex(span.span_context.trace_id)}")
    println("Span ID: ${bytes_to_hex(span.span_context.span_id)}")
    println("Kind: ${span_kind_to_string(span.kind)}")
    println("Start Time: ${span.start_time_unix_nanos}")
    
    match span.end_time_unix_nanos {
      Some(end_time) => {
        println("End Time: ${end_time}")
        println("Duration: ${end_time - span.start_time_unix_nanos}")
      }
      None => println("End Time: (not ended)")
    }
    
    if span.attributes.length() > 0 {
      println("Attributes:")
      for (key, value) in span.attributes {
        println("  ${key}: ${attribute_value_to_string(value)}")
      }
    }
    
    if span.events.length() > 0 {
      println("Events:")
      for event in span.events {
        println("  ${event.name} at ${event.time_unix_nanos}")
        for (key, value) in event.attributes {
          println("    ${key}: ${attribute_value_to_string(value)}")
        }
      }
    }
    
    if span.links.length() > 0 {
      println("Links:")
      for link in span.links {
        println("  -> ${bytes_to_hex(link.span_context.trace_id)}/${bytes_to_hex(link.span_context.span_id)}")
      }
    }
    
    println("Status: ${status_code_to_string(span.status.code)}")
    if span.status.description != "" {
      println("Status Description: ${span.status.description}")
    }
    
    println("")
  } else {
    // Compact format
    let trace_id = bytes_to_hex(span.span_context.trace_id)
    let span_id = bytes_to_hex(span.span_context.span_id)
    let duration = match span.end_time_unix_nanos {
      Some(end_time) => "${end_time - span.start_time_unix_nanos}"
      None => "ongoing"
    }
    println("[${trace_id} ${span_id}] ${span.name} (${duration})")
  }
}

// Convert span kind to string
fn span_kind_to_string(kind : SpanKind) -> String {
  match kind {
    Internal => "Internal"
    Server => "Server"
    Client => "Client"
    Producer => "Producer"
    Consumer => "Consumer"
  }
}

// Convert status code to string
fn status_code_to_string(code : StatusCode) -> String {
  match code {
    Unset => "Unset"
    Ok => "Ok"
    Error => "Error"
  }
}

// Convert attribute value to string
fn attribute_value_to_string(value : @common.AttributeValue) -> String {
  match value {
    @common::AttributeValue::String(s) => "\"${s}\""
    @common::AttributeValue::Int64(i) => "${i}"
    @common::AttributeValue::Float64(f) => "${f}"
    @common::AttributeValue::Bool(b) => "${b}"
    @common::AttributeValue::ArrayString(arr) => "${arr}"
    @common::AttributeValue::ArrayInt64(arr) => "${arr}"
    @common::AttributeValue::ArrayFloat64(arr) => "${arr}"
    @common::AttributeValue::ArrayBool(arr) => "${arr}"
  }
}

// Export result
pub enum ExportResult {
  Success
  Failure(String)
}

// Span exporter interface
pub trait SpanExporter {
  async fn export(self, spans : Array[ReadableSpan]) -> ExportResult
  async fn force_flush(self) -> ExportResult
  async fn shutdown(self) -> ExportResult
}