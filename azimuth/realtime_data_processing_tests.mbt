// Real-time Data Processing Test Suite for Azimuth Telemetry System
// Test cases covering real-time telemetry data processing and streaming scenarios

test "real_time_metrics_aggregation" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "realtime.metrics")
  
  // Create metrics for real-time monitoring
  let request_counter = Meter::create_counter(meter, "realtime.requests", Some("Real-time request counter"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "realtime.response_time", Some("Real-time response times"), Some("ms"))
  let error_gauge = Meter::create_gauge(meter, "realtime.error_rate", Some("Real-time error rate"), Some("percent"))
  let active_connections = Meter::create_updown_counter(meter, "realtime.active_connections", Some("Active connections"), Some("connections"))
  
  // Simulate real-time data stream
  let time_windows = 10
  let requests_per_window = 50
  
  for window = 0; window < time_windows; window = window + 1 {
    let window_start = Clock::now_unix_nanos(Clock::system())
    
    // Process requests in this time window
    for request = 0; request < requests_per_window; request = request + 1 {
      let request_attrs = Attributes::new()
      Attributes::set(request_attrs, "window", IntValue(window))
      Attributes::set(request_attrs, "request.id", IntValue(request))
      Attributes::set(request_attrs, "endpoint", StringValue("/api/realtime/data"))
      
      // Record request
      Counter::add(request_counter, 1.0, Some(request_attrs))
      
      // Simulate response time based on load
      let base_response_time = 100.0
      let load_factor = (window * 10).to_double()
      let response_time = base_response_time + load_factor + (request % 20).to_double()
      
      let response_attrs = Attributes::new()
      Attributes::set(response_attrs, "window", IntValue(window))
      Attributes::set(response_attrs, "load.factor", StringValue(load_factor.to_string()))
      
      Histogram::record(response_histogram, response_time, Some(response_attrs))
      
      // Simulate errors (10% error rate)
      if request % 10 == 0 {
        let error_attrs = Attributes::new()
        Attributes::set(error_attrs, "window", IntValue(window))
        Attributes::set(error_attrs, "error.type", StringValue("timeout"))
        
        // In real implementation, would use Gauge::set to update error rate
        // Gauge::set(error_gauge, 10.0, Some(error_attrs))
        
        // Log error
        let logger_provider = LoggerProvider::default()
        let logger = LoggerProvider::get_logger(logger_provider, "realtime.errors")
        let error_log = LogRecord::new(Error, "Real-time processing error in window " + window.to_string())
        Logger::emit(logger, error_log)
      }
      
      // Simulate connection management
      if request % 5 == 0 {
        // Add connection
        Counter::add(active_connections, 1.0, Some(request_attrs))
      }
      
      if request % 15 == 0 {
        // Remove connection (in real implementation, would use negative value)
        // Counter::add(active_connections, -1.0, Some(request_attrs))
      }
    }
    
    let window_end = Clock::now_unix_nanos(Clock::system())
    let window_duration = window_end - window_start
    
    // Create window summary log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "realtime.summary")
    let summary_log = LogRecord::new_with_context(
      Info,
      Some("Window " + window.to_string() + " completed: " + requests_per_window.to_string() + " requests processed"),
      Some(Attributes::new()),
      Some(window_start),
      Some(window_end),
      Some("realtime_trace_" + window.to_string()),
      Some("realtime_span_" + window.to_string()),
      None
    )
    Logger::emit(logger, summary_log)
  }
  
  // Verify metrics were created
  assert_eq(request_counter.name, "realtime.requests")
  assert_eq(response_histogram.name, "realtime.response_time")
  assert_eq(error_gauge.name, "realtime.error_rate")
  assert_eq(active_connections.name, "realtime.active_connections")
}

test "streaming_telemetry_pipeline" {
  // Create telemetry components for streaming pipeline
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "streaming.pipeline")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "streaming.pipeline")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "streaming.pipeline")
  
  // Create pipeline metrics
  let throughput_counter = Meter::create_counter(meter, "streaming.throughput", Some("Messages processed"), Some("messages"))
  let latency_histogram = Meter::create_histogram(meter, "streaming.latency", Some("Processing latency"), Some("ms"))
  let backlog_gauge = Meter::create_gauge(meter, "streaming.backlog", Some("Message backlog"), Some("messages"))
  
  // Simulate streaming data pipeline stages
  let pipeline_stages = ["ingestion", "parsing", "validation", "transformation", "enrichment", "output"]
  let messages_per_stage = 20
  
  for stage_index = 0; stage_index < pipeline_stages.length(); stage_index = stage_index + 1 {
    let stage_name = pipeline_stages[stage_index]
    
    // Create span for this pipeline stage
    let stage_span = Tracer::start_span(tracer, "streaming.stage." + stage_name)
    let stage_context = Span::span_context(stage_span)
    
    Span::add_event(stage_span, "stage.started", Some([
      ("stage.name", StringValue(stage_name)),
      ("stage.index", IntValue(stage_index)),
      ("expected.messages", IntValue(messages_per_stage))
    ]))
    
    // Process messages in this stage
    for message_index = 0; message_index < messages_per_stage; message_index = message_index + 1 {
      let message_start = Clock::now_unix_nanos(Clock::system())
      let message_id = "msg_" + stage_index.to_string() + "_" + message_index.to_string()
      
      // Create message context
      let message_attrs = Attributes::new()
      Attributes::set(message_attrs, "message.id", StringValue(message_id))
      Attributes::set(message_attrs, "stage", StringValue(stage_name))
      Attributes::set(message_attrs, "pipeline.version", StringValue("1.0.0"))
      
      // Record throughput
      Counter::add(throughput_counter, 1.0, Some(message_attrs))
      
      // Simulate message processing
      let processing_time = 10.0 + (message_index % 50).to_double()
      Histogram::record(latency_histogram, processing_time, Some(message_attrs))
      
      // Log message processing
      let message_log = LogRecord::new_with_context(
        Debug,
        Some("Processing message " + message_id + " in stage " + stage_name),
        Some(message_attrs),
        Some(message_start),
        Some(Clock::now_unix_nanos(Clock::system())),
        Some(SpanContext::trace_id(stage_context)),
        Some(SpanContext::span_id(stage_context)),
        None
      )
      Logger::emit(logger, message_log)
      
      // Simulate backlog updates
      let backlog_size = (messages_per_stage - message_index).to_double()
      let backlog_attrs = Attributes::new()
      Attributes::set(backlog_attrs, "stage", StringValue(stage_name))
      // In real implementation, would use Gauge::set
      // Gauge::set(backlog_gauge, backlog_size, Some(backlog_attrs))
      
      // Add stage-specific events
      match stage_name {
        "ingestion" => {
          Span::add_event(stage_span, "message.received", Some([
            ("message.id", StringValue(message_id)),
            ("source", StringValue("kafka")),
            ("topic", StringValue("telemetry.data"))
          ]))
        }
        "validation" => {
          let is_valid = message_index % 5 != 0 // 80% valid
          Span::add_event(stage_span, "message.validated", Some([
            ("message.id", StringValue(message_id)),
            ("valid", BoolValue(is_valid)),
            ("validation.rules", StringValue("schema,format,required"))
          ]))
          
          if not is_valid {
            let error_log = LogRecord::new(Warn, "Message validation failed for " + message_id)
            Logger::emit(logger, error_log)
          }
        }
        "output" => {
          Span::add_event(stage_span, "message.output", Some([
            ("message.id", StringValue(message_id)),
            ("destination", StringValue("elasticsearch")),
            ("index", StringValue("telemetry-" + stage_index.to_string()))
          ]))
        }
        _ => {
          Span::add_event(stage_span, "message.processed", Some([
            ("message.id", StringValue(message_id)),
            ("operation", StringValue("transform"))
          ]))
        }
      }
    }
    
    // Complete stage
    Span::add_event(stage_span, "stage.completed", Some([
      ("stage.name", StringValue(stage_name)),
      ("messages.processed", IntValue(messages_per_stage))
    ]))
    
    Span::end(stage_span)
    
    // Log stage completion
    let stage_log = LogRecord::new(Info, "Pipeline stage '" + stage_name + "' completed successfully")
    Logger::emit(logger, stage_log)
  }
  
  // Verify pipeline metrics
  assert_eq(throughput_counter.name, "streaming.throughput")
  assert_eq(latency_histogram.name, "streaming.latency")
  assert_eq(backlog_gauge.name, "streaming.backlog")
}

test "real_time_anomaly_detection" {
  // Create telemetry for anomaly detection system
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "anomaly.detection")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "anomaly.detection")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "anomaly.detection")
  
  // Create anomaly detection metrics
  let data_points_counter = Meter::create_counter(meter, "anomaly.data_points", Some("Data points analyzed"), Some("points"))
  let anomalies_counter = Meter::create_counter(meter, "anomalies.detected", Some("Anomalies detected"), Some("anomalies"))
  let severity_histogram = Meter::create_histogram(meter, "anomaly.severity", Some("Anomaly severity scores"), Some("score"))
  
  // Simulate real-time data stream with anomalies
  let detection_span = Tracer::start_span(tracer, "realtime.anomaly.detection")
  
  let data_points = 100
  let anomaly_threshold = 80.0
  let detected_anomalies = 0
  
  for point = 0; point < data_points; point = point + 1 {
    let point_start = Clock::now_unix_nanos(Clock::system())
    let point_id = "point_" + point.to_string()
    
    // Simulate data point metrics
    let base_value = 50.0
    let random_variation = (point % 20).to_double()
    let data_value = base_value + random_variation
    
    // Inject anomalies at specific points
    let anomaly_score = if point % 15 == 0 {
      // Simulate anomaly
      90.0 + (point % 10).to_double()
    } else {
      // Normal data point
      random_variation
    }
    
    // Record data point
    let point_attrs = Attributes::new()
    Attributes::set(point_attrs, "point.id", StringValue(point_id))
    Attributes::set(point_attrs, "data.value", StringValue(data_value.to_string()))
    Attributes::set(point_attrs, "timestamp", StringValue("2025-01-01T12:00:" + (point % 60).to_string()))
    
    Counter::add(data_points_counter, 1.0, Some(point_attrs))
    Histogram::record(severity_histogram, anomaly_score, Some(point_attrs))
    
    // Check for anomaly
    let is_anomaly = anomaly_score > anomaly_threshold
    if is_anomaly {
      detected_anomalies = detected_anomalies + 1
      
      // Record anomaly
      let anomaly_attrs = Attributes::new()
      Attributes::set(anomaly_attrs, "point.id", StringValue(point_id))
      Attributes::set(anomaly_attrs, "anomaly.score", StringValue(anomaly_score.to_string()))
      Attributes::set(anomaly_attrs, "anomaly.type", StringValue("statistical"))
      Attributes::set(anomaly_attrs, "threshold", StringValue(anomaly_threshold.to_string()))
      
      Counter::add(anomalies_counter, 1.0, Some(anomaly_attrs))
      
      // Log anomaly detection
      let anomaly_log = LogRecord::new_with_context(
        Warn,
        Some("Anomaly detected in data point " + point_id + " with score " + anomaly_score.to_string()),
        Some(anomaly_attrs),
        Some(point_start),
        Some(Clock::now_unix_nanos(Clock::system())),
        Some(SpanContext::trace_id(Span::span_context(detection_span))),
        Some(SpanContext::span_id(Span::span_context(detection_span))),
        None
      )
      Logger::emit(logger, anomaly_log)
      
      // Add anomaly event to span
      Span::add_event(detection_span, "anomaly.detected", Some([
        ("point.id", StringValue(point_id)),
        ("anomaly.score", StringValue(anomaly_score.to_string())),
        ("data.value", StringValue(data_value.to_string()))
      ]))
    } else {
      // Log normal data point
      let normal_log = LogRecord::new(Debug, "Normal data point processed: " + point_id)
      Logger::emit(logger, normal_log)
    }
    
    // Simulate real-time processing delay
    let processing_time = 1.0 + (point % 5).to_double()
    let processing_attrs = Attributes::new()
    Attributes::set(processing_attrs, "point.id", StringValue(point_id))
    Attributes::set(processing_attrs, "processing.time", StringValue(processing_time.to_string()))
    
    let processing_histogram = Meter::create_histogram(meter, "anomaly.processing_time", Some("Processing time"), Some("ms"))
    Histogram::record(processing_histogram, processing_time, Some(processing_attrs))
  }
  
  // Complete detection span
  Span::add_event(detection_span, "detection.completed", Some([
    ("total.points", IntValue(data_points)),
    ("anomalies.detected", IntValue(detected_anomalies)),
    ("anomaly.rate", StringValue((detected_anomalies.to_double() / data_points.to_double() * 100.0).to_string() + "%"))
  ]))
  
  Span::end(detection_span)
  
  // Log detection summary
  let summary_log = LogRecord::new(Info, "Anomaly detection completed: " + detected_anomalies.to_string() + " anomalies found out of " + data_points.to_string() + " data points")
  Logger::emit(logger, summary_log)
  
  // Verify anomaly detection metrics
  assert_eq(data_points_counter.name, "anomaly.data_points")
  assert_eq(anomalies_counter.name, "anomalies.detected")
  assert_eq(severity_histogram.name, "anomaly.severity")
  
  // Verify we detected expected number of anomalies
  let expected_anomalies = data_points / 15
  assert_eq(detected_anomalies, expected_anomalies)
}

test "real_time_dashboard_data_streaming" {
  // Create telemetry for dashboard data streaming
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "dashboard.streaming")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "dashboard.streaming")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "dashboard.streaming")
  
  // Create dashboard metrics
  let update_counter = Meter::create_counter(meter, "dashboard.updates", Some("Dashboard updates"), Some("updates"))
  let latency_histogram = Meter::create_histogram(meter, "dashboard.update_latency", Some("Update latency"), Some("ms"))
  let active_users_gauge = Meter::create_gauge(meter, "dashboard.active_users", Some("Active users"), Some("users"))
  
  // Simulate real-time dashboard data streaming
  let streaming_span = Tracer::start_span(tracer, "dashboard.realtime.streaming")
  
  let dashboard_widgets = ["user.metrics", "system.health", "performance.charts", "error.rates", "business.kpis"]
  let update_cycles = 20
  
  for cycle = 0; cycle < update_cycles; cycle = cycle + 1 {
    let cycle_start = Clock::now_unix_nanos(Clock::system())
    
    Span::add_event(streaming_span, "update.cycle.started", Some([
      ("cycle.number", IntValue(cycle)),
      ("widgets.count", IntValue(dashboard_widgets.length()))
    ]))
    
    // Update each widget
    for widget_index = 0; widget_index < dashboard_widgets.length(); widget_index = widget_index + 1 {
      let widget_name = dashboard_widgets[widget_index]
      let widget_start = Clock::now_unix_nanos(Clock::system())
      
      // Create widget-specific data
      let widget_attrs = Attributes::new()
      Attributes::set(widget_attrs, "widget.name", StringValue(widget_name))
      Attributes::set(widget_attrs, "cycle", IntValue(cycle))
      Attributes::set(widget_attrs, "dashboard.id", StringValue("main_dashboard"))
      
      // Simulate data generation based on widget type
      match widget_name {
        "user.metrics" => {
          let active_users = 1000 + (cycle * 50) + (widget_index * 100)
          Attributes::set(widget_attrs, "active.users", IntValue(active_users))
          Attributes::set(widget_attrs, "new.users", IntValue(cycle * 5))
          
          // In real implementation, would use Gauge::set
          // Gauge::set(active_users_gauge, active_users.to_double(), Some(widget_attrs))
        }
        "system.health" => {
          let cpu_usage = 50.0 + (cycle % 40).to_double()
          let memory_usage = 60.0 + (cycle % 30).to_double()
          Attributes::set(widget_attrs, "cpu.usage", StringValue(cpu_usage.to_string()))
          Attributes::set(widget_attrs, "memory.usage", StringValue(memory_usage.to_string()))
        }
        "performance.charts" => {
          let response_time = 100.0 + (cycle % 200).to_double()
          let throughput = 1000.0 - (cycle % 200).to_double()
          Attributes::set(widget_attrs, "response.time", StringValue(response_time.to_string()))
          Attributes::set(widget_attrs, "throughput", StringValue(throughput.to_string()))
        }
        "error.rates" => {
          let error_rate = (cycle % 10).to_double()
          let error_count = cycle * 2
          Attributes::set(widget_attrs, "error.rate", StringValue(error_rate.to_string()))
          Attributes::set(widget_attrs, "error.count", IntValue(error_count))
        }
        "business.kpis" => {
          let revenue = 10000.0 + (cycle * 500.0)
          let orders = 100 + (cycle * 10)
          Attributes::set(widget_attrs, "revenue", StringValue(revenue.to_string()))
          Attributes::set(widget_attrs, "orders", IntValue(orders))
        }
        _ => {
          Attributes::set(widget_attrs, "data.type", StringValue("generic"))
        }
      }
      
      // Record widget update
      Counter::add(update_counter, 1.0, Some(widget_attrs))
      
      let widget_end = Clock::now_unix_nanos(Clock::system())
      let update_latency = (widget_end - widget_start).to_double() / 1000000.0 // Convert to milliseconds
      
      let latency_attrs = Attributes::new()
      Attributes::set(latency_attrs, "widget.name", StringValue(widget_name))
      Histogram::record(latency_histogram, update_latency, Some(latency_attrs))
      
      // Log widget update
      let widget_log = LogRecord::new_with_context(
        Debug,
        Some("Updated widget '" + widget_name + "' in cycle " + cycle.to_string()),
        Some(widget_attrs),
        Some(widget_start),
        Some(widget_end),
        Some(SpanContext::trace_id(Span::span_context(streaming_span))),
        Some(SpanContext::span_id(Span::span_context(streaming_span))),
        None
      )
      Logger::emit(logger, widget_log)
      
      // Add widget update event
      Span::add_event(streaming_span, "widget.updated", Some([
        ("widget.name", StringValue(widget_name)),
        ("update.latency.ms", StringValue(update_latency.to_string())),
        ("cycle", IntValue(cycle))
      ]))
    }
    
    let cycle_end = Clock::now_unix_nanos(Clock::system())
    let cycle_duration = (cycle_end - cycle_start).to_double() / 1000000.0 // Convert to milliseconds
    
    Span::add_event(streaming_span, "update.cycle.completed", Some([
      ("cycle.number", IntValue(cycle)),
      ("cycle.duration.ms", StringValue(cycle_duration.to_string())),
      ("widgets.updated", IntValue(dashboard_widgets.length()))
    ]))
    
    // Log cycle completion
    let cycle_log = LogRecord::new(Info, "Dashboard update cycle " + cycle.to_string() + " completed in " + cycle_duration.to_string() + "ms")
    Logger::emit(logger, cycle_log)
  }
  
  // Complete streaming span
  Span::end(streaming_span)
  
  // Log streaming completion
  let completion_log = LogRecord::new(Info, "Dashboard streaming completed: " + (update_cycles * dashboard_widgets.length()).to_string() + " total updates")
  Logger::emit(logger, completion_log)
  
  // Verify dashboard metrics
  assert_eq(update_counter.name, "dashboard.updates")
  assert_eq(latency_histogram.name, "dashboard.update_latency")
  assert_eq(active_users_gauge.name, "dashboard.active_users")
}

test "real_time_alert_processing" {
  // Create telemetry for alert processing system
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "alert.processing")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "alert.processing")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "alert.processing")
  
  // Create alert processing metrics
  let alerts_counter = Meter::create_counter(meter, "alerts.received", Some("Alerts received"), Some("alerts"))
  let processing_histogram = Meter::create_histogram(meter, "alert.processing_time", Some("Alert processing time"), Some("ms"))
  let escalation_counter = Meter::create_counter(meter, "alerts.escalated", Some("Alerts escalated"), Some("escalations"))
  
  // Simulate real-time alert processing
  let processing_span = Tracer::start_span(tracer, "realtime.alert.processing")
  
  let alert_types = ["system.down", "high.error.rate", "performance.degradation", "security.breach", "resource.exhaustion"]
  let alert_severities = ["low", "medium", "high", "critical"]
  let alerts_count = 50
  
  for alert_index = 0; alert_index < alerts_count; alert_index = alert_index + 1 {
    let alert_start = Clock::now_unix_nanos(Clock::system())
    let alert_id = "alert_" + alert_index.to_string()
    
    // Generate alert properties
    let alert_type = alert_types[alert_index % alert_types.length()]
    let alert_severity = alert_severities[alert_index % alert_severities.length()]
    
    let alert_attrs = Attributes::new()
    Attributes::set(alert_attrs, "alert.id", StringValue(alert_id))
    Attributes::set(alert_attrs, "alert.type", StringValue(alert_type))
    Attributes::set(alert_attrs, "alert.severity", StringValue(alert_severity))
    Attributes::set(alert_attrs, "source.service", StringValue("service_" + (alert_index % 5).to_string()))
    Attributes::set(alert_attrs, "timestamp", StringValue("2025-01-01T12:00:" + (alert_index % 60).to_string()))
    
    // Record alert reception
    Counter::add(alerts_counter, 1.0, Some(alert_attrs))
    
    // Log alert reception
    let alert_log = LogRecord::new_with_context(
      Warn,
      Some("Alert received: " + alert_type + " with severity: " + alert_severity),
      Some(alert_attrs),
      Some(alert_start),
      None,
      Some(SpanContext::trace_id(Span::span_context(processing_span))),
      Some(SpanContext::span_id(Span::span_context(processing_span))),
      None
    )
    Logger::emit(logger, alert_log)
    
    // Add alert received event
    Span::add_event(processing_span, "alert.received", Some([
      ("alert.id", StringValue(alert_id)),
      ("alert.type", StringValue(alert_type)),
      ("alert.severity", StringValue(alert_severity))
    ]))
    
    // Simulate alert processing
    let processing_steps = ["validation", "enrichment", "correlation", "action.decision", "notification"]
    
    for step_index = 0; step_index < processing_steps.length(); step_index = step_index + 1 {
      let step_name = processing_steps[step_index]
      let step_start = Clock::now_unix_nanos(Clock::system())
      
      // Simulate step processing
      let step_duration = 5.0 + (step_index * 2).to_double() + (alert_index % 10).to_double()
      
      // Add step-specific attributes
      match step_name {
        "validation" => {
          Attributes::set(alert_attrs, "validation.result", StringValue("passed"))
          Attributes::set(alert_attrs, "validation.rules", StringValue("format,required,range"))
        }
        "enrichment" => {
          Attributes::set(alert_attrs, "enriched.data", StringValue("service.owner,team,escalation.policy"))
          Attributes::set(alert_attrs, "service.owner", StringValue("team_" + (alert_index % 3).to_string()))
        }
        "correlation" => {
          let correlated_alerts = alert_index % 5
          Attributes::set(alert_attrs, "correlated.alerts", IntValue(correlated_alerts))
        }
        "action.decision" => {
          let should_escalate = alert_severity == "high" || alert_severity == "critical"
          Attributes::set(alert_attrs, "escalation.required", BoolValue(should_escalate))
          
          if should_escalate {
            let escalation_attrs = Attributes::new()
            Attributes::set(escalation_attrs, "alert.id", StringValue(alert_id))
            Attributes::set(escalation_attrs, "escalation.reason", StringValue("high.severity"))
            Counter::add(escalation_counter, 1.0, Some(escalation_attrs))
          }
        }
        "notification" => {
          let notification_channels = ["email", "slack", "pagerduty"]
          let selected_channel = notification_channels[alert_index % notification_channels.length()]
          Attributes::set(alert_attrs, "notification.channel", StringValue(selected_channel))
        }
        _ => {
          Attributes::set(alert_attrs, "step.status", StringValue("completed"))
        }
      }
      
      let step_end = Clock::now_unix_nanos(Clock::system())
      
      // Log step completion
      let step_log = LogRecord::new(Debug, "Alert processing step '" + step_name + "' completed for " + alert_id)
      Logger::emit(logger, step_log)
      
      // Add step event
      Span::add_event(processing_span, "alert.step.completed", Some([
        ("alert.id", StringValue(alert_id)),
        ("step.name", StringValue(step_name)),
        ("step.duration.ms", StringValue(step_duration.to_string()))
      ]))
    }
    
    let alert_end = Clock::now_unix_nanos(Clock::system())
    let total_processing_time = (alert_end - alert_start).to_double() / 1000000.0 // Convert to milliseconds
    
    // Record processing time
    let processing_time_attrs = Attributes::new()
    Attributes::set(processing_time_attrs, "alert.type", StringValue(alert_type))
    Attributes::set(processing_time_attrs, "alert.severity", StringValue(alert_severity))
    Histogram::record(processing_histogram, total_processing_time, Some(processing_time_attrs))
    
    // Log alert processing completion
    let completion_log = LogRecord::new(Info, "Alert " + alert_id + " processed in " + total_processing_time.to_string() + "ms")
    Logger::emit(logger, completion_log)
    
    // Add alert completed event
    Span::add_event(processing_span, "alert.processed", Some([
      ("alert.id", StringValue(alert_id)),
      ("total.processing.time.ms", StringValue(total_processing_time.to_string()))
    ]))
  }
  
  // Complete processing span
  Span::end(processing_span)
  
  // Log processing completion
  let final_log = LogRecord::new(Info, "Alert processing batch completed: " + alerts_count.to_string() + " alerts processed")
  Logger::emit(logger, final_log)
  
  // Verify alert processing metrics
  assert_eq(alerts_counter.name, "alerts.received")
  assert_eq(processing_histogram.name, "alert.processing_time")
  assert_eq(escalation_counter.name, "alerts.escalated")
}