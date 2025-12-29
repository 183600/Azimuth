// telemetry/api/propagation/w3c_trace_context.mbt

// W3CTraceContextPropagator implements the W3C Trace Context propagation
pub type W3CTraceContextPropagator {}

impl TextMapPropagator for W3CTraceContextPropagator {
  fn inject(self, ctx : @context.Context, carrier : TextMapCarrier) -> Unit {
    match get_span_context(ctx) {
      Some(span_ctx) => {
        let trace_parent = format_trace_parent(span_ctx)
        carrier.set("traceparent", trace_parent)
        
        if span_ctx.trace_state != "" {
          carrier.set("tracestate", span_ctx.trace_state)
        }
      }
      None => () // No span context to inject
    }
  }
  
  fn extract(self, ctx : @context.Context, carrier : TextMapCarrier) -> @context.Context {
    match carrier.get("traceparent") {
      Some(trace_parent) => {
        match parse_trace_parent(trace_parent) {
          Ok(span_ctx) => {
            let trace_state = carrier.get("tracestate").unwrap_or("")
            let final_span_ctx = { span_ctx | trace_state: trace_state, is_remote: true }
            set_span_context(ctx, final_span_ctx)
          }
          Err(_) => ctx // Invalid traceparent, return original context
        }
      }
      None => ctx // No traceparent header
    }
  }
  
  fn fields(self) -> Array[String] {
    ["traceparent", "tracestate"]
  }
}

// Context key for storing span context
let SPAN_CONTEXT_KEY : @context.ContextKey[SpanContext] = { key: "span_context" }

// Get span context from context
pub fn get_span_context(ctx : @context.Context) -> Option[SpanContext] {
  @context::get(ctx, SPAN_CONTEXT_KEY)
}

// Set span context in context
pub fn set_span_context(ctx : @context.Context, span_ctx : SpanContext) -> @context.Context {
  @context::with_value(ctx, SPAN_CONTEXT_KEY, span_ctx)
}

// Format traceparent header according to W3C specification
fn format_trace_parent(span_ctx : SpanContext) -> String {
  let version = "00" // Version 2
  let trace_id = bytes_to_hex(span_ctx.trace_id)
  let span_id = bytes_to_hex(span_ctx.span_id)
  let flags = byte_to_hex(span_ctx.trace_flags)
  "${version}-${trace_id}-${span_id}-${flags}"
}

// Parse traceparent header according to W3C specification
fn parse_trace_parent(trace_parent : String) -> Result[SpanContext, String] {
  let parts = trace_parent.split("-")
  if parts.length() != 4 {
    return Err("Invalid traceparent format")
  }
  
  let version = parts[0]
  if version != "00" && version != "ff" {
    return Err("Unsupported traceparent version")
  }
  
  match (hex_to_bytes(parts[1]), hex_to_bytes(parts[2]), hex_to_byte(parts[3])) {
    (Ok(trace_id), Ok(span_id), Ok(flags)) => {
      if trace_id.length() != 16 || span_id.length() != 8 {
        return Err("Invalid trace_id or span_id length")
      }
      
      Ok({
        trace_id: trace_id,
        span_id: span_id,
        trace_flags: flags,
        trace_state: "",
        is_remote: false
      })
    }
    _ => Err("Invalid hex encoding")
  }
}

// Convert bytes to hex string
fn bytes_to_hex(bytes : Array[Byte]) -> String {
  let mut result = ""
  for byte in bytes {
    result = result + byte_to_hex(byte)
  }
  result
}

// Convert a single byte to hex string
fn byte_to_hex(byte : Byte) -> String {
  let hex_chars = "0123456789abcdef"
  let high = (byte >> 4) & 0x0f
  let low = byte & 0x0f
  "${hex_chars[high]}${hex_chars[low]}"
}

// Convert hex string to bytes
fn hex_to_bytes(hex : String) -> Result[Array[Byte], String] {
  if hex.length() % 2 != 0 {
    return Err("Hex string must have even length")
  }
  
  let mut bytes = Array::new()
  let mut i = 0
  while i < hex.length() {
    let high_char = hex[i]
    let low_char = hex[i + 1]
    
    match (hex_char_to_nibble(high_char), hex_char_to_nibble(low_char)) {
      (Ok(high), Ok(low)) => {
        let byte = (high << 4) | low
        bytes.push(byte)
      }
      _ => return Err("Invalid hex character")
    }
    i = i + 2
  }
  
  Ok(bytes)
}

// Convert hex string to single byte
fn hex_to_byte(hex : String) -> Result[Byte, String] {
  if hex.length() != 2 {
    return Err("Hex byte must have exactly 2 characters")
  }
  
  match (hex_char_to_nibble(hex[0]), hex_char_to_nibble(hex[1])) {
    (Ok(high), Ok(low)) => Ok((high << 4) | low)
    _ => Err("Invalid hex character")
  }
}

// Convert hex character to nibble (4 bits)
fn hex_char_to_nibble(c : Char) -> Result[Byte, String] {
  if '0' <= c && c <= '9' {
    Ok((c - '0') as Byte)
  } else if 'a' <= c && c <= 'f' {
    Ok((c - 'a' + 10) as Byte)
  } else if 'A' <= c && c <= 'F' {
    Ok((c - 'A' + 10) as Byte)
  } else {
    Err("Invalid hex character")
  }
}