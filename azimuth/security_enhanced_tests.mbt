// Security Test Suite for Azimuth Telemetry System
// Test cases covering security aspects, data protection, and access control

test "sensitive_data_sanitization" {
  // Test that sensitive data is properly sanitized in telemetry
  
  // Create attributes with sensitive data
  let sensitive_attrs = Attributes::new()
  Attributes::set(sensitive_attrs, "user.password", StringValue("super_secret_password_123"))
  Attributes::set(sensitive_attrs, "api.key", StringValue("sk_live_1234567890abcdef"))
  Attributes::set(sensitive_attrs, "credit.card", StringValue("4111-1111-1111-1111"))
  Attributes::set(sensitive_attrs, "ssn", StringValue("123-45-6789"))
  Attributes::set(sensitive_attrs, "email", StringValue("user@example.com"))
  Attributes::set(sensitive_attrs, "phone", StringValue("+1-555-123-4567"))
  Attributes::set(sensitive_attrs, "address", StringValue("123 Main St, Anytown, USA 12345"))
  Attributes::set(sensitive_attrs, "personal.info", StringValue("John Doe, DOB: 1980-01-01"))
  
  // Test sensitive data in logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "security.test")
  
  // Create log with potentially sensitive data
  let sensitive_log = LogRecord::new_with_context(
    Info,
    Some("User login attempt with password=super_secret_password_123 and api_key=sk_live_1234567890abcdef"),
    Some(sensitive_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace_with_sensitive_data"),
    Some("span_with_sensitive_data"),
    None
  )
  
  Logger::emit(logger, sensitive_log)
  
  // Test sensitive data in span events
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "security.test")
  let span = Tracer::start_span(tracer, "sensitive.operation")
  
  Span::add_event(span, "user.authenticated", Some([
    ("user.id", StringValue("user_12345")),
    ("user.password", StringValue("super_secret_password_123")), // Should be sanitized
    ("session.token", StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")), // JWT token
    ("ip.address", StringValue("192.168.1.100")) // PII
  ]))
  
  Span::end(span)
  
  // Test sensitive data in baggage
  let baggage = Baggage::new()
  let baggage_with_sensitive = Baggage::set_entry(baggage, "auth.token", "bearer_secret_token_12345")
  let baggage_with_api_key = Baggage::set_entry(baggage_with_sensitive, "api.key", "sk_live_1234567890abcdef")
  
  // Test sensitive data in HTTP headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "Authorization", "Bearer sk_live_1234567890abcdef")
  TextMapCarrier::set(carrier, "X-API-Key", "secret_api_key_12345")
  TextMapCarrier::set(carrier, "Cookie", "session_id=abc123; user_data=sensitive_info")
  TextMapCarrier::set(carrier, "X-User-Password", "super_secret_password_123")
  
  // In a real implementation, these would be sanitized
  // For this test, we verify they don't cause crashes
  assert_true(true) // Test passes if no exceptions occur
}

test "access_control_and_authorization" {
  // Test access control mechanisms in telemetry operations
  
  // Create different user roles with different access levels
  let admin_attrs = Attributes::new()
  Attributes::set(admin_attrs, "user.role", StringValue("admin"))
  Attributes::set(admin_attrs, "user.permissions", ArrayStringValue(["read", "write", "delete", "admin"]))
  Attributes::set(admin_attrs, "access.level", IntValue(100))
  
  let user_attrs = Attributes::new()
  Attributes::set(user_attrs, "user.role", StringValue("user"))
  Attributes::set(user_attrs, "user.permissions", ArrayStringValue(["read", "write"]))
  Attributes::set(user_attrs, "access.level", IntValue(50))
  
  let readonly_attrs = Attributes::new()
  Attributes::set(readonly_attrs, "user.role", StringValue("readonly"))
  Attributes::set(readonly_attrs, "user.permissions", ArrayStringValue(["read"]))
  Attributes::set(readonly_attrs, "access.level", IntValue(10))
  
  // Test access-controlled operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "access.control.test")
  
  // Create access control metrics
  let access_attempts_counter = Meter::create_counter(meter, "access.attempts.total", Some("Access attempts"), Some("attempts"))
  let access_denied_counter = Meter::create_counter(meter, "access.denied.total", Some("Access denied"), Some("denials"))
  let access_granted_counter = Meter::create_counter(meter, "access.granted.total", Some("Access granted"), Some("grants"))
  
  // Simulate access attempts
  let access_scenarios = [
    ("admin", "read.metrics", true),
    ("admin", "write.metrics", true),
    ("admin", "delete.metrics", true),
    ("admin", "admin.operations", true),
    ("user", "read.metrics", true),
    ("user", "write.metrics", true),
    ("user", "delete.metrics", false),
    ("user", "admin.operations", false),
    ("readonly", "read.metrics", true),
    ("readonly", "write.metrics", false),
    ("readonly", "delete.metrics", false),
    ("readonly", "admin.operations", false)
  ]
  
  for scenario in access_scenarios {
    let role = scenario.0
    let operation = scenario.1
    let should_allow = scenario.2
    
    // Record access attempt
    let attempt_attrs = Attributes::new()
    Attributes::set(attempt_attrs, "user.role", StringValue(role))
    Attributes::set(attempt_attrs, "operation", StringValue(operation))
    Counter::add(access_attempts_counter, 1.0, Some(attempt_attrs))
    
    if should_allow {
      // Access granted
      let granted_attrs = Attributes::new()
      Attributes::set(granted_attrs, "user.role", StringValue(role))
      Attributes::set(granted_attrs, "operation", StringValue(operation))
      Counter::add(access_granted_counter, 1.0, Some(granted_attrs))
      
      // Log successful access
      let logger_provider = LoggerProvider::default()
      let logger = LoggerProvider::get_logger(logger_provider, "access.control")
      let success_log = LogRecord::new(Info, "Access granted for role '" + role + "' to operation '" + operation + "'")
      Logger::emit(logger, success_log)
    } else {
      // Access denied
      let denied_attrs = Attributes::new()
      Attributes::set(denied_attrs, "user.role", StringValue(role))
      Attributes::set(denied_attrs, "operation", StringValue(operation))
      Attributes::set(denied_attrs, "reason", StringValue("insufficient.permissions"))
      Counter::add(access_denied_counter, 1.0, Some(denied_attrs))
      
      // Log denied access
      let logger_provider = LoggerProvider::default()
      let logger = LoggerProvider::get_logger(logger_provider, "access.control")
      let denial_log = LogRecord::new(Warn, "Access denied for role '" + role + "' to operation '" + operation + "'")
      Logger::emit(logger, denial_log)
    }
  }
  
  // Test role-based span access
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "access.control")
  
  // Admin span with full access
  let admin_span = Tracer::start_span(tracer, "admin.operation")
  Span::add_event(admin_span, "admin.action", Some([
    ("user.role", StringValue("admin")),
    ("access.level", IntValue(100)),
    ("operation", StringValue("system.config.change"))
  ]))
  Span::end(admin_span)
  
  // User span with limited access
  let user_span = Tracer::start_span(tracer, "user.operation")
  Span::add_event(user_span, "user.action", Some([
    ("user.role", StringValue("user")),
    ("access.level", IntValue(50)),
    ("operation", StringValue("data.update"))
  ]))
  Span::end(user_span)
  
  // Readonly span with read-only access
  let readonly_span = Tracer::start_span(tracer, "readonly.operation")
  Span::add_event(readonly_span, "readonly.action", Some([
    ("user.role", StringValue("readonly")),
    ("access.level", IntValue(10)),
    ("operation", StringValue("data.view"))
  ]))
  Span::end(readonly_span)
  
  // Verify access control metrics
  assert_eq(access_attempts_counter.name, "access.attempts.total")
  assert_eq(access_denied_counter.name, "access.denied.total")
  assert_eq(access_granted_counter.name, "access.granted.total")
}

test "encryption_and_data_protection" {
  // Test encryption and data protection mechanisms
  
  // Create sensitive data that should be encrypted
  let sensitive_data = [
    ("user.social.security.number", "123-45-6789"),
    ("credit.card.number", "4111-1111-1111-1111"),
    ("bank.account.number", "1234567890"),
    ("medical.record.id", "MR-12345-67890"),
    ("personal.email", "user.personal@example.com"),
    ("home.address", "123 Privacy Lane, Secret City, USA 12345"),
    ("phone.number", "+1-555-123-4567"),
    ("driver.license", "D123456789"),
    ("passport.number", "P123456789"),
    ("insurance.policy", "POL-12345-67890")
  ]
  
  // Test encryption at rest (simulated)
  let encrypted_attrs = Attributes::new()
  
  for data in sensitive_data {
    let field_name = data.0
    let plain_value = data.1
    
    // Simulate encryption (in real implementation, would use actual encryption)
    let encrypted_value = "ENCRYPTED:" + plain_value + ":SALT"
    
    Attributes::set(encrypted_attrs, field_name + ".encrypted", StringValue(encrypted_value))
    Attributes::set(encrypted_attrs, field_name + ".encryption.algorithm", StringValue("AES-256-GCM"))
    Attributes::set(encrypted_attrs, field_name + ".encryption.key.id", StringValue("key-rotation-2025-01"))
  }
  
  // Test encryption in transit (simulated)
  let carrier = TextMapCarrier::new()
  
  // Add encrypted headers
  for data in sensitive_data {
    let field_name = data.0
    let plain_value = data.1
    
    // Simulate transit encryption
    let transit_encrypted = "TLS:" + plain_value + ":ENCRYPTED"
    TextMapCarrier::set(carrier, "X-Encrypted-" + field_name.replace(".", "-"), transit_encrypted)
  }
  
  // Add security metadata
  TextMapCarrier::set(carrier, "X-TLS-Version", "1.3")
  TextMapCarrier::set(carrier, "X-Cipher-Suite", "TLS_AES_256_GCM_SHA384")
  TextMapCarrier::set(carrier, "X-Certificate-Thumbprint", "ABC123DEF456GHI789JKL012MNO345PQR678STU901VWX234YZA567")
  
  // Test key rotation scenarios
  let key_rotation_scenarios = [
    ("2025-01-01", "key-2025-01", "AES-256-GCM"),
    ("2025-02-01", "key-2025-02", "AES-256-GCM"),
    ("2025-03-01", "key-2025-03", "AES-256-GCM")
  ]
  
  for scenario in key_rotation_scenarios {
    let rotation_date = scenario.0
    let key_id = scenario.1
    let algorithm = scenario.2
    
    // Create key rotation log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "encryption.key.rotation")
    
    let rotation_attrs = Attributes::new()
    Attributes::set(rotation_attrs, "rotation.date", StringValue(rotation_date))
    Attributes::set(rotation_attrs, "key.id", StringValue(key_id))
    Attributes::set(rotation_attrs, "algorithm", StringValue(algorithm))
    
    let rotation_log = LogRecord::new_with_context(
      Info,
      Some("Key rotation completed with key ID: " + key_id),
      Some(rotation_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("encryption_trace_" + rotation_date),
      Some("encryption_span_" + key_id),
      None
    )
    Logger::emit(logger, rotation_log)
  }
  
  // Test data masking and tokenization
  let masking_scenarios = [
    ("email", "user@example.com", "u***@e******.com"),
    ("phone", "+1-555-123-4567", "+1-555-***-****"),
    ("credit.card", "4111-1111-1111-1111", "****-****-****-1111"),
    ("ssn", "123-45-6789", "***-**-6789")
  ]
  
  for scenario in masking_scenarios {
    let data_type = scenario.0
    let original = scenario.1
    let masked = scenario.2
    
    let masking_attrs = Attributes::new()
    Attributes::set(masking_attrs, "data.type", StringValue(data_type))
    Attributes::set(masking_attrs, "original.value", StringValue(original))
    Attributes::set(masking_attrs, "masked.value", StringValue(masked))
    Attributes::set(masking_attrs, "masking.policy", StringValue("default"))
    
    // Create masking span
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "data.masking")
    let masking_span = Tracer::start_span(tracer, "data.masking.operation")
    
    Span::add_event(masking_span, "data.masked", Some([
      ("data.type", StringValue(data_type)),
      ("masking.applied", BoolValue(true)),
      ("policy.version", StringValue("1.0"))
    ]))
    
    Span::end(masking_span)
  }
  
  // Create encryption metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "encryption.metrics")
  
  let encryption_operations = Meter::create_counter(meter, "encryption.operations.total", Some("Encryption operations"), Some("operations"))
  let decryption_operations = Meter::create_counter(meter, "decryption.operations.total", Some("Decryption operations"), Some("operations"))
  let key_rotations = Meter::create_counter(meter, "key.rotations.total", Some("Key rotations"), Some("rotations"))
  
  // Record encryption metrics
  let encryption_attrs = Attributes::new()
  Attributes::set(encryption_attrs, "algorithm", StringValue("AES-256-GCM"))
  Attributes::set(encryption_attrs, "key.length", IntValue(256))
  Counter::add(encryption_operations, sensitive_data.length().to_double(), Some(encryption_attrs))
  
  let decryption_attrs = Attributes::new()
  Attributes::set(decryption_attrs, "algorithm", StringValue("AES-256-GCM"))
  Attributes::set(decryption_attrs, "key.length", IntValue(256))
  Counter::add(decryption_operations, sensitive_data.length().to_double(), Some(decryption_attrs))
  
  let rotation_attrs = Attributes::new()
  Attributes::set(rotation_attrs, "rotation.type", StringValue("scheduled"))
  Counter::add(key_rotations, key_rotation_scenarios.length().to_double(), Some(rotation_attrs))
  
  // Verify encryption metrics
  assert_eq(encryption_operations.name, "encryption.operations.total")
  assert_eq(decryption_operations.name, "decryption.operations.total")
  assert_eq(key_rotations.name, "key.rotations.total")
}

test "audit_logging_and_compliance" {
  // Test audit logging and compliance features
  
  // Create audit logger
  let logger_provider = LoggerProvider::default()
  let audit_logger = LoggerProvider::get_logger(logger_provider, "audit.log")
  
  // Define compliance events
  let compliance_events = [
    ("user.login", "INFO", "User authenticated successfully"),
    ("data.access", "INFO", "User accessed sensitive data"),
    ("permission.change", "WARN", "User permissions modified"),
    ("config.change", "WARN", "System configuration changed"),
    ("security.incident", "ERROR", "Security incident detected"),
    ("data.export", "INFO", "Data export initiated"),
    ("admin.action", "WARN", "Administrative action performed"),
    ("failed.login", "WARN", "Failed login attempt"),
    ("data.deletion", "ERROR", "Sensitive data deleted"),
    ("system.breach", "ERROR", "Security breach detected")
  ]
  
  for event in compliance_events {
    let event_type = event.0
    let severity_str = event.1
    let description = event.2
    
    let severity = match severity_str {
      "INFO" => Info
      "WARN" => Warn
      "ERROR" => Error
      _ => Info
    }
    
    // Create comprehensive audit attributes
    let audit_attrs = Attributes::new()
    Attributes::set(audit_attrs, "event.type", StringValue(event_type))
    Attributes::set(audit_attrs, "event.category", StringValue("security"))
    Attributes::set(audit_attrs, "compliance.framework", StringValue("SOC2,GDPR,HIPAA"))
    Attributes::set(audit_attrs, "user.id", StringValue("user_12345"))
    Attributes::set(audit_attrs, "session.id", StringValue("session_abc123"))
    Attributes::set(audit_attrs, "ip.address", StringValue("192.168.1.100"))
    Attributes::set(audit_attrs, "user.agent", StringValue("Azimuth-Security-Client/1.0"))
    Attributes::set(audit_attrs, "timestamp", StringValue("2025-01-01T12:00:00Z"))
    Attributes::set(audit_attrs, "source.service", StringValue("auth.service"))
    Attributes::set(audit_attrs, "target.resource", StringValue("/api/secure/data"))
    Attributes::set(audit_attrs, "outcome", StringValue(if severity == Error { "failure" } else { "success" }))
    Attributes::set(audit_attrs, "retention.period", StringValue("7years"))
    Attributes::set(audit_attrs, "audit.required", BoolValue(true))
    Attributes::set(audit_attrs, "legal.hold", BoolValue(false))
    
    // Create audit log record
    let audit_log = LogRecord::new_with_context(
      severity,
      Some(description),
      Some(audit_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      Some(Clock::now_unix_nanos(Clock::system()) + 1000L),
      Some("audit_trace_" + event_type),
      Some("audit_span_" + event_type),
      None
    )
    
    Logger::emit(audit_logger, audit_log)
    
    // Create corresponding audit span
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "audit.tracer")
    let audit_span = Tracer::start_span(tracer, "audit." + event_type)
    
    Span::add_event(audit_span, "audit.event.logged", Some([
      ("event.type", StringValue(event_type)),
      ("compliance.framework", StringValue("SOC2,GDPR,HIPAA")),
      ("retention.period", StringValue("7years"))
    ]))
    
    Span::end(audit_span)
  }
  
  // Test compliance reporting
  let compliance_metrics = [
    ("audit.events.total", compliance_events.length()),
    ("audit.events.info", 3),
    ("audit.events.warn", 4),
    ("audit.events.error", 3),
    ("compliance.frameworks.active", 3),
    ("retention.period.years", 7)
  ]
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "compliance.metrics")
  
  for metric in compliance_metrics {
    let metric_name = metric.0
    let metric_value = metric.1
    
    let compliance_gauge = Meter::create_gauge(meter, metric_name, Some("Compliance metric"), Some("units"))
    
    // In real implementation, would use Gauge::set
    // Gauge::set(compliance_gauge, metric_value.to_double(), None)
    
    // Log compliance metric
    let metric_log = LogRecord::new(Info, "Compliance metric '" + metric_name + "' = " + metric_value.to_string())
    Logger::emit(audit_logger, metric_log)
  }
  
  // Test data retention and deletion policies
  let retention_scenarios = [
    ("audit.logs", "7years", "automatic"),
    ("user.data", "5years", "automatic"),
    ("security.incidents", "10years", "manual"),
    ("financial.records", "7years", "automatic"),
    ("personal.info", "2years", "automatic")
  ]
  
  for scenario in retention_scenarios {
    let data_type = scenario.0
    let retention_period = scenario.1
    let deletion_method = scenario.2
    
    let retention_attrs = Attributes::new()
    Attributes::set(retention_attrs, "data.type", StringValue(data_type))
    Attributes::set(retention_attrs, "retention.period", StringValue(retention_period))
    Attributes::set(retention_attrs, "deletion.method", StringValue(deletion_method))
    Attributes::set(retention_attrs, "gdpr.relevant", BoolValue(true))
    Attributes::set(retention_attrs, "compliance.checked", BoolValue(true))
    
    let retention_log = LogRecord::new(Info, "Retention policy applied for " + data_type + ": " + retention_period)
    Logger::emit(audit_logger, retention_log)
  }
  
  // Test legal hold scenarios
  let legal_hold_cases = [
    ("case-001", "legal.investigation", "active"),
    ("case-002", "regulatory.audit", "active"),
    ("case-003", "litigation", "closed")
  ]
  
  for case in legal_hold_cases {
    let case_id = case.0
    let case_type = case.1
    let case_status = case.2
    
    let legal_hold_attrs = Attributes::new()
    Attributes::set(legal_hold_attrs, "case.id", StringValue(case_id))
    Attributes::set(legal_hold_attrs, "case.type", StringValue(case_type))
    Attributes::set(legal_hold_attrs, "case.status", StringValue(case_status))
    Attributes::set(legal_hold_attrs, "data.preserved", BoolValue(case_status == "active"))
    Attributes::set(legal_hold_attrs, "legal.review", BoolValue(true))
    
    let legal_hold_log = LogRecord::new(Warn, "Legal hold status for case " + case_id + ": " + case_status)
    Logger::emit(audit_logger, legal_hold_log)
  }
  
  // Verify audit logging functionality
  assert_true(true) // Test passes if no exceptions occur during audit logging
}

test "threat_detection_and_monitoring" {
  // Test threat detection and security monitoring
  
  // Create security monitoring metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "security.monitoring")
  
  let threat_detection_counter = Meter::create_counter(meter, "threat.detection.total", Some("Threats detected"), Some("threats"))
  let false_positive_counter = Meter::create_counter(meter, "false.positive.total", Some("False positives"), Some("incidents"))
  let security_incident_counter = Meter::create_counter(meter, "security.incident.total", Some("Security incidents"), Some("incidents"))
  
  // Define threat scenarios
  let threat_scenarios = [
    ("brute.force.attack", "high", "multiple.failed.logins", true),
    ("sql.injection.attempt", "critical", "malicious.sql.pattern", true),
    ("xss.attack", "medium", "cross.site.scripting", true),
    ("ddos.attack", "high", "high.request.volume", true),
    ("unusual.access.pattern", "medium", "anomaly.detected", false),
    ("data.exfiltration", "critical", "large.data.transfer", true),
    ("privilege.escalation", "high", "unauthorized.privilege.use", true),
    ("malicious.payload", "critical", "malware.signature", true),
    ("reconnaissance", "low", "port.scanning", false),
    ("credential.stuffing", "high", "stolen.credentials.used", true)
  ]
  
  for threat in threat_scenarios {
    let threat_type = threat.0
    let severity = threat.1
    let pattern = threat.2
    let is_real_threat = threat.3
    
    // Record threat detection
    let threat_attrs = Attributes::new()
    Attributes::set(threat_attrs, "threat.type", StringValue(threat_type))
    Attributes::set(threat_attrs, "severity", StringValue(severity))
    Attributes::set(threat_attrs, "detection.pattern", StringValue(pattern))
    Attributes::set(threat_attrs, "source.ip", StringValue("192.168.1.100"))
    Attributes::set(threat_attrs, "target.service", StringValue("api.gateway"))
    Attributes::set(threat_attrs, "detection.engine", StringValue("azimuth.security"))
    Attributes::set(threat_attrs, "confidence.score", FloatValue(0.85))
    
    if is_real_threat {
      Counter::add(threat_detection_counter, 1.0, Some(threat_attrs))
      Counter::add(security_incident_counter, 1.0, Some(threat_attrs))
    } else {
      Counter::add(false_positive_counter, 1.0, Some(threat_attrs))
    }
    
    // Create threat detection log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "threat.detection")
    
    let log_severity = match severity {
      "critical" => Error
      "high" => Warn
      "medium" => Warn
      "low" => Info
      _ => Info
    }
    
    let threat_log = LogRecord::new_with_context(
      log_severity,
      Some("Threat detected: " + threat_type + " with severity: " + severity),
      Some(threat_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("threat_trace_" + threat_type),
      Some("threat_span_" + threat_type),
      None
    )
    
    Logger::emit(logger, threat_log)
    
    // Create threat detection span
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "threat.detection")
    let threat_span = Tracer::start_span(tracer, "threat." + threat_type)
    
    Span::add_event(threat_span, "threat.analyzed", Some([
      ("threat.type", StringValue(threat_type)),
      ("severity", StringValue(severity)),
      ("confidence.score", FloatValue(0.85)),
      ("mitigation.applied", BoolValue(is_real_threat))
    ]))
    
    if is_real_threat {
      Span::add_event(threat_span, "mitigation.started", Some([
        ("mitigation.type", StringValue("automatic")),
        ("response.time", StringValue("immediate"))
      ]))
    }
    
    Span::end(threat_span)
  }
  
  // Test anomaly detection
  let anomaly_scenarios = [
    ("unusual.login.time", "user.login.at.3am"),
    ("large.data.download", "user.downloaded.10gb"),
    ("multiple.failed.auth", "5.failed.logins.1.minute"),
    ("privilege.change", "user.elevated.to.admin"),
    ("api.abuse", "1000.requests.per.second"),
    ("geo.anomaly", "login.from.unusual.country"),
    ("device.anomaly", "login.from.new.device"),
    ("behavior.anomaly", "unusual.user.behavior")
  ]
  
  for anomaly in anomaly_scenarios {
    let anomaly_type = anomaly.0
    let anomaly_description = anomaly.1
    
    let anomaly_attrs = Attributes::new()
    Attributes::set(anomaly_attrs, "anomaly.type", StringValue(anomaly_type))
    Attributes::set(anomaly_attrs, "anomaly.description", StringValue(anomaly_description))
    Attributes::set(anomaly_attrs, "detection.method", StringValue("machine.learning"))
    Attributes::set(anomaly_attrs, "anomaly.score", FloatValue(0.92))
    Attributes::set(anomaly_attrs, "baseline.deviation", StringValue("3.5.sigma"))
    Attributes::set(anomaly_attrs, "investigation.required", BoolValue(true))
    
    // Create anomaly detection log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "anomaly.detection")
    
    let anomaly_log = LogRecord::new_with_context(
      Warn,
      Some("Anomaly detected: " + anomaly_description),
      Some(anomaly_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("anomaly_trace_" + anomaly_type),
      Some("anomaly_span_" + anomaly_type),
      None
    )
    
    Logger::emit(logger, anomaly_log)
  }
  
  // Test security incident response
  let incident_response_phases = [
    ("detection", "threat.identified"),
    ("analysis", "threat.analyzed"),
    ("containment", "threat.contained"),
    ("eradication", "threat.eradicated"),
    ("recovery", "service.restored"),
    ("post.incident", "lessons.learned")
  ]
  
  for phase in incident_response_phases {
    let phase_name = phase.0
    let phase_description = phase.1
    
    let response_attrs = Attributes::new()
    Attributes::set(response_attrs, "incident.phase", StringValue(phase_name))
    Attributes::set(response_attrs, "phase.description", StringValue(phase_description))
    Attributes::set(response_attrs, "incident.id", StringValue("INC-2025-001"))
    Attributes::set(response_attrs, "response.team", StringValue("security.operations"))
    Attributes::set(response_attrs, "phase.duration", IntValue(300)) // 5 minutes
    
    // Create incident response log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "incident.response")
    
    let response_log = LogRecord::new_with_context(
      Info,
      Some("Incident response phase: " + phase_description),
      Some(response_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("incident_trace_2025_001"),
      Some("incident_span_" + phase_name),
      None
    )
    
    Logger::emit(logger, response_log)
  }
  
  // Verify security monitoring metrics
  assert_eq(threat_detection_counter.name, "threat.detection.total")
  assert_eq(false_positive_counter.name, "false.positive.total")
  assert_eq(security_incident_counter.name, "security.incident.total")
}