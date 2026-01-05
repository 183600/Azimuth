// Azimuth Advanced Security Test Suite
// This file contains test cases for advanced security functionality

// Test 1: Threat Detection and Response
test "threat detection and response" {
  // Define threat detection structure
  type SecurityEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    severity: String,
    source_ip: String,
    target_service: String,
    description: String,
    indicators: Array[ThreatIndicator],
    metadata: Array[(String, String)]
  }
  
  type ThreatIndicator = {
    indicator_type: String,
    value: String,
    confidence: Float,
    source: String
  }
  
  type ThreatRule = {
    rule_id: String,
    name: String,
    condition: String,
    severity: String,
    action: String,
    enabled: Bool
  }
  
  type ThreatResponse = {
    response_id: String,
    event_id: String,
    rule_id: String,
    action: String,
    status: String,
    timestamp: Int,
    details: Array[(String, String)]
  }
  
  // Create security events
  let security_events = [
    {
      event_id: "sec-event-001",
      timestamp: 1640995200,
      event_type: "authentication_failure",
      severity: "medium",
      source_ip: "192.168.1.100",
      target_service: "api-gateway",
      description: "Multiple failed login attempts",
      indicators: [
        {
          indicator_type: "ip_reputation",
          value: "suspicious",
          confidence: 0.7,
          source: "threat-intelligence-feed"
        },
        {
          indicator_type: "failed_attempts",
          value: "5",
          confidence: 1.0,
          source: "authentication-service"
        }
      ],
      metadata: [
        ("user_agent", "curl/7.68.0"),
        ("endpoint", "/api/v1/auth/login"),
        ("username", "admin")
      ]
    },
    {
      event_id: "sec-event-002",
      timestamp: 1640995210,
      event_type: "sql_injection_attempt",
      severity: "high",
      source_ip: "10.0.0.50",
      target_service: "user-service",
      description: "SQL injection pattern detected in query parameters",
      indicators: [
        {
          indicator_type: "sql_pattern",
          value: "UNION SELECT",
          confidence: 0.9,
          source: "waf-engine"
        },
        {
          indicator_type: "anomalous_query",
          value: "true",
          confidence: 0.8,
          source: "query-analyzer"
        }
      ],
      metadata: [
        ("user_agent", "Mozilla/5.0 (compatible; scanner/1.0)"),
        ("endpoint", "/api/v1/users"),
        ("query", "id=1 UNION SELECT username,password FROM users")
      ]
    },
    {
      event_id: "sec-event-003",
      timestamp: 1640995220,
      event_type: "data_exfiltration",
      severity: "critical",
      source_ip: "172.16.0.25",
      target_service: "data-service",
      description: "Large volume of data being downloaded from internal service",
      indicators: [
        {
          indicator_type: "data_volume",
          value: "5GB",
          confidence: 0.95,
          source: "traffic-monitor"
        },
        {
          indicator_type: "unusual_time",
          value: "true",
          confidence: 0.7,
          source: "behavioral-analysis"
        }
      ],
      metadata: [
        ("user_agent", "custom-client/1.0"),
        ("endpoint", "/api/v1/export"),
        ("duration", "1800s")
      ]
    }
  ]
  
  // Create threat detection rules
  let threat_rules = [
    {
      rule_id: "brute-force-detection",
      name: "Brute Force Attack Detection",
      condition: "event_type == 'authentication_failure' AND indicators.failed_attempts >= 5",
      severity: "medium",
      action: "block_ip_temporarily",
      enabled: true
    },
    {
      rule_id: "sql-injection-detection",
      name: "SQL Injection Attack Detection",
      condition: "event_type == 'sql_injection_attempt' AND indicators.sql_pattern.confidence >= 0.8",
      severity: "high",
      action: "block_ip_permanently",
      enabled: true
    },
    {
      rule_id: "data-exfiltration-detection",
      name: "Data Exfiltration Detection",
      condition: "event_type == 'data_exfiltration' AND indicators.data_volume.confidence >= 0.9",
      severity: "critical",
      action: "block_ip_and_alert",
      enabled: true
    }
  ]
  
  // Filter events by severity
  let filter_events_by_severity = fn(events: Array[SecurityEvent], severity: String) {
    events.filter(fn(e) { e.severity == severity })
  }
  
  let critical_events = filter_events_by_severity(security_events, "critical")
  assert_eq(critical_events.length(), 1)
  assert_eq(critical_events[0].event_id, "sec-event-003")
  
  let high_events = filter_events_by_severity(security_events, "high")
  assert_eq(high_events.length(), 1)
  assert_eq(high_events[0].event_id, "sec-event-002")
  
  let medium_events = filter_events_by_severity(security_events, "medium")
  assert_eq(medium_events.length(), 1)
  assert_eq(medium_events[0].event_id, "sec-event-001")
  
  // Check if event matches rule condition
  let event_matches_rule = fn(event: SecurityEvent, rule: ThreatRule) {
    match rule.rule_id {
      "brute-force-detection" => {
        event.event_type == "authentication_failure" and
        event.indicators.any(fn(i) { 
          i.indicator_type == "failed_attempts" and i.value.to_int() >= 5 
        })
      }
      
      "sql-injection-detection" => {
        event.event_type == "sql_injection_attempt" and
        event.indicators.any(fn(i) { 
          i.indicator_type == "sql_pattern" and i.confidence >= 0.8 
        })
      }
      
      "data-exfiltration-detection" => {
        event.event_type == "data_exfiltration" and
        event.indicators.any(fn(i) { 
          i.indicator_type == "data_volume" and i.confidence >= 0.9 
        })
      }
      
      _ => false
    }
  }
  
  // Test rule matching
  let brute_force_rule = threat_rules.filter(fn(r) { r.rule_id == "brute-force-detection" })[0]
  let auth_event = security_events.filter(fn(e) { e.event_id == "sec-event-001" })[0]
  assert_true(event_matches_rule(auth_event, brute_force_rule))
  
  let sql_injection_rule = threat_rules.filter(fn(r) { r.rule_id == "sql-injection-detection" })[0]
  let sql_event = security_events.filter(fn(e) { e.event_id == "sec-event-002" })[0]
  assert_true(event_matches_rule(sql_event, sql_injection_rule))
  
  let data_exfil_rule = threat_rules.filter(fn(r) { r.rule_id == "data-exfiltration-detection" })[0]
  let exfil_event = security_events.filter(fn(e) { e.event_id == "sec-event-003" })[0]
  assert_true(event_matches_rule(exfil_event, data_exfil_rule))
  
  // Generate threat response
  let generate_response = fn(event: SecurityEvent, rule: ThreatRule) {
    let response_id = "resp-" + event.event_id
    let action = rule.action
    let status = "executed"
    let timestamp = 1640995300
    
    let details = match action {
      "block_ip_temporarily" => [
        ("ip_address", event.source_ip),
        ("duration", "3600"),
        ("reason", "Brute force attack detected")
      ]
      
      "block_ip_permanently" => [
        ("ip_address", event.source_ip),
        ("duration", "permanent"),
        ("reason", "SQL injection attack detected")
      ]
      
      "block_ip_and_alert" => [
        ("ip_address", event.source_ip),
        ("duration", "permanent"),
        ("reason", "Data exfiltration detected"),
        ("alert_sent", "true")
      ]
      
      _ => []
    }
    
    {
      response_id,
      event_id: event.event_id,
      rule_id: rule.rule_id,
      action,
      status,
      timestamp,
      details
    }
  }
  
  // Generate responses for all matching events
  let generate_all_responses = fn(events: Array[SecurityEvent], rules: Array[ThreatRule]) {
    let mut responses = []
    
    for event in events {
      for rule in rules {
        if rule.enabled and event_matches_rule(event, rule) {
          responses = responses.push(generate_response(event, rule))
        }
      }
    }
    
    responses
  }
  
  let threat_responses = generate_all_responses(security_events, threat_rules)
  assert_eq(threat_responses.length(), 3)
  
  // Verify responses
  let brute_force_response = threat_responses.filter(fn(r) { r.rule_id == "brute-force-detection" })[0]
  assert_eq(brute_force_response.event_id, "sec-event-001")
  assert_eq(brute_force_response.action, "block_ip_temporarily")
  assert_eq(brute_force_response.details.filter(fn(d) { d.0 == "ip_address" })[0].1, "192.168.1.100")
  
  let sql_response = threat_responses.filter(fn(r) { r.rule_id == "sql-injection-detection" })[0]
  assert_eq(sql_response.event_id, "sec-event-002")
  assert_eq(sql_response.action, "block_ip_permanently")
  assert_eq(sql_response.details.filter(fn(d) { d.0 == "ip_address" })[0].1, "10.0.0.50")
  
  let exfil_response = threat_responses.filter(fn(r) { r.rule_id == "data-exfiltration-detection" })[0]
  assert_eq(exfil_response.event_id, "sec-event-003")
  assert_eq(exfil_response.action, "block_ip_and_alert")
  assert_eq(exfil_response.details.filter(fn(d) { d.0 == "alert_sent" })[0].1, "true")
  
  // Calculate threat detection metrics
  let calculate_threat_metrics = fn(events: Array[SecurityEvent], responses: Array[ThreatResponse]) {
    let total_events = events.length()
    
    let events_by_severity = [
      ("critical", events.filter(fn(e) { e.severity == "critical" }).length()),
      ("high", events.filter(fn(e) { e.severity == "high" }).length()),
      ("medium", events.filter(fn(e) { e.severity == "medium" }).length()),
      ("low", events.filter(fn(e) { e.severity == "low" }).length())
    ]
    
    let total_responses = responses.length()
    let response_rate = if total_events > 0 {
      (total_responses.to_float() / total_events.to_float()) * 100.0
    } else {
      0.0
    }
    
    let actions_by_type = [
      ("block_ip_temporarily", responses.filter(fn(r) { r.action == "block_ip_temporarily" }).length()),
      ("block_ip_permanently", responses.filter(fn(r) { r.action == "block_ip_permanently" }).length()),
      ("block_ip_and_alert", responses.filter(fn(r) { r.action == "block_ip_and_alert" }).length())
    ]
    
    let unique_ips = {
      let mut ips = []
      for event in events {
        if not(ips.contains(event.source_ip)) {
          ips = ips.push(event.source_ip)
        }
      }
      ips.length()
    }
    
    {
      total_events,
      events_by_severity,
      total_responses,
      response_rate,
      actions_by_type,
      unique_ips
    }
  }
  
  let threat_metrics = calculate_threat_metrics(security_events, threat_responses)
  assert_eq(threat_metrics.total_events, 3)
  assert_eq(threat_metrics.events_by_severity.filter(fn(s) { s.0 == "critical" })[0].1, 1)
  assert_eq(threat_metrics.events_by_severity.filter(fn(s) { s.0 == "high" })[0].1, 1)
  assert_eq(threat_metrics.events_by_severity.filter(fn(s) { s.0 == "medium" })[0].1, 1)
  assert_eq(threat_metrics.total_responses, 3)
  assert_eq(threat_metrics.response_rate, 100.0)
  assert_eq(threat_metrics.actions_by_type.filter(fn(a) { a.0 == "block_ip_temporarily" })[0].1, 1)
  assert_eq(threat_metrics.actions_by_type.filter(fn(a) { a.0 == "block_ip_permanently" })[0].1, 1)
  assert_eq(threat_metrics.actions_by_type.filter(fn(a) { a.0 == "block_ip_and_alert" })[0].1, 1)
  assert_eq(threat_metrics.unique_ips, 3)
}

// Test 2: Encryption and Key Management
test "encryption and key management" {
  // Define encryption key structure
  type EncryptionKey = {
    key_id: String,
    algorithm: String,
    key_size: Int,
    created_at: Int,
    expires_at: Int,
    status: String,
    usage_count: Int,
    max_usage: Int
  }
  
  type EncryptionOperation = {
    operation_id: String,
    key_id: String,
    operation_type: String,
    input_data: String,
    output_data: String,
    timestamp: Int,
    success: Bool
  }
  
  type KeyRotationPolicy = {
    policy_id: String,
    name: String,
    rotation_interval_days: Int,
    usage_threshold: Int,
    age_threshold_days: Int,
    auto_rotate: Bool
  }
  
  // Create encryption keys
  let encryption_keys = [
    {
      key_id: "key-001",
      algorithm: "AES-256-GCM",
      key_size: 256,
      created_at: 1640995200,
      expires_at: 1643587200,  // 30 days later
      status: "active",
      usage_count: 100,
      max_usage: 10000
    },
    {
      key_id: "key-002",
      algorithm: "RSA-2048",
      key_size: 2048,
      created_at: 1640995200,
      expires_at: 1704067200,  // 1 year later
      status: "active",
      usage_count: 50,
      max_usage: 5000
    },
    {
      key_id: "key-003",
      algorithm: "AES-256-GCM",
      key_size: 256,
      created_at: 1638316800,  // 30 days ago
      expires_at: 1640908800,  // Expired
      status: "expired",
      usage_count: 8000,
      max_usage: 10000
    }
  ]
  
  // Create key rotation policies
  let rotation_policies = [
    {
      policy_id: "policy-001",
      name: "Standard Key Rotation",
      rotation_interval_days: 90,
      usage_threshold: 8000,
      age_threshold_days: 80,
      auto_rotate: true
    },
    {
      policy_id: "policy-002",
      name: "High-Frequency Rotation",
      rotation_interval_days: 30,
      usage_threshold: 5000,
      age_threshold_days: 25,
      auto_rotate: true
    }
  ]
  
  // Filter keys by status
  let filter_keys_by_status = fn(keys: Array[EncryptionKey], status: String) {
    keys.filter(fn(k) { k.status == status })
  }
  
  let active_keys = filter_keys_by_status(encryption_keys, "active")
  assert_eq(active_keys.length(), 2)
  
  let expired_keys = filter_keys_by_status(encryption_keys, "expired")
  assert_eq(expired_keys.length(), 1)
  assert_eq(expired_keys[0].key_id, "key-003")
  
  // Filter keys by algorithm
  let filter_keys_by_algorithm = fn(keys: Array[EncryptionKey], algorithm: String) {
    keys.filter(fn(k) { k.algorithm == algorithm })
  }
  
  let aes_keys = filter_keys_by_algorithm(encryption_keys, "AES-256-GCM")
  assert_eq(aes_keys.length(), 2)
  
  let rsa_keys = filter_keys_by_algorithm(encryption_keys, "RSA-2048")
  assert_eq(rsa_keys.length(), 1)
  
  // Check if key needs rotation
  let key_needs_rotation = fn(key: EncryptionKey, policy: KeyRotationPolicy, current_time: Int) {
    let age_days = (current_time - key.created_at) / 86400
    let usage_ratio = key.usage_count.to_float() / key.max_usage.to_float()
    
    let age_threshold_reached = age_days >= policy.age_threshold_days
    let usage_threshold_reached = usage_ratio >= (policy.usage_threshold.to_float() / key.max_usage.to_float())
    let expired = current_time >= key.expires_at
    
    age_threshold_reached or usage_threshold_reached or expired
  }
  
  // Test key rotation needs
  let current_time = 1640995200
  let standard_policy = rotation_policies.filter(fn(p) { p.policy_id == "policy-001" })[0]
  
  let key001 = encryption_keys.filter(fn(k) { k.key_id == "key-001" })[0]
  assert_false(key_needs_rotation(key001, standard_policy, current_time))  // New key, low usage
  
  let key003 = encryption_keys.filter(fn(k) { k.key_id == "key-003" })[0]
  assert_true(key_needs_rotation(key003, standard_policy, current_time))  // Expired key
  
  // Test with high-frequency policy
  let high_freq_policy = rotation_policies.filter(fn(p) { p.policy_id == "policy-002" })[0]
  assert_false(key_needs_rotation(key001, high_freq_policy, current_time))  // Still doesn't need rotation
  
  // Simulate encryption operation
  let simulate_encryption = fn(key: EncryptionKey, data: String) {
    let success = key.status == "active" and key.usage_count < key.max_usage
    
    if success {
      // Simulate encryption by just returning a modified string
      let encrypted_data = "encrypted:" + data + ":with:" + key.key_id
      {
        operation_id: "op-" + key.key_id + "-" + key.usage_count.to_string(),
        key_id: key.key_id,
        operation_type: "encrypt",
        input_data: data,
        output_data: encrypted_data,
        timestamp: 1640995250,
        success: true
      }
    } else {
      {
        operation_id: "op-" + key.key_id + "-failed",
        key_id: key.key_id,
        operation_type: "encrypt",
        input_data: data,
        output_data: "",
        timestamp: 1640995250,
        success: false
      }
    }
  }
  
  // Simulate decryption operation
  let simulate_decryption = fn(key: EncryptionKey, encrypted_data: String) {
    let success = key.status == "active" and 
                  encrypted_data.starts_with("encrypted:") and 
                  encrypted_data.contains(":with:" + key.key_id)
    
    if success {
      // Simulate decryption by extracting the original data
      let parts = encrypted_data.split(":")
      let decrypted_data = if parts.length() >= 3 { parts[1] } else { "" }
      
      {
        operation_id: "op-" + key.key_id + "-decrypt-" + key.usage_count.to_string(),
        key_id: key.key_id,
        operation_type: "decrypt",
        input_data: encrypted_data,
        output_data: decrypted_data,
        timestamp: 1640995250,
        success: true
      }
    } else {
      {
        operation_id: "op-" + key.key_id + "-decrypt-failed",
        key_id: key.key_id,
        operation_type: "decrypt",
        input_data: encrypted_data,
        output_data: "",
        timestamp: 1640995250,
        success: false
      }
    }
  }
  
  // Test encryption with active key
  let active_key = active_keys[0]
  let encryption_result = simulate_encryption(active_key, "sensitive data")
  assert_true(encryption_result.success)
  assert_eq(encryption_result.key_id, active_key.key_id)
  assert_eq(encryption_result.output_data, "encrypted:sensitive data:with:" + active_key.key_id)
  
  // Test decryption with correct key
  let decryption_result = simulate_decryption(active_key, encryption_result.output_data)
  assert_true(decryption_result.success)
  assert_eq(decryption_result.output_data, "sensitive data")
  
  // Test encryption with expired key
  let expired_key = expired_keys[0]
  let expired_encryption_result = simulate_encryption(expired_key, "sensitive data")
  assert_false(expired_encryption_result.success)
  
  // Test decryption with wrong key
  let wrong_key_decryption = simulate_decryption(active_keys[1], encryption_result.output_data)
  assert_false(wrong_key_decryption.success)
  
  // Update key usage count
  let update_key_usage = fn(keys: Array[EncryptionKey], key_id: String) {
    keys.map(fn(k) {
      if k.key_id == key_id and k.status == "active" {
        { k | usage_count: k.usage_count + 1 }
      } else {
        k
      }
    })
  }
  
  let updated_keys = update_key_usage(encryption_keys, active_key.key_id)
  let updated_key = updated_keys.filter(fn(k) { k.key_id == active_key.key_id })[0]
  assert_eq(updated_key.usage_count, active_key.usage_count + 1)
  
  // Calculate key management metrics
  let calculate_key_metrics = fn(keys: Array[EncryptionKey], operations: Array[EncryptionOperation]) {
    let total_keys = keys.length()
    
    let keys_by_status = [
      ("active", keys.filter(fn(k) { k.status == "active" }).length()),
      ("expired", keys.filter(fn(k) { k.status == "expired" }).length()),
      ("revoked", keys.filter(fn(k) { k.status == "revoked" }).length())
    ]
    
    let keys_by_algorithm = [
      ("AES-256-GCM", keys.filter(fn(k) { k.algorithm == "AES-256-GCM" }).length()),
      ("RSA-2048", keys.filter(fn(k) { k.algorithm == "RSA-2048" }).length())
    ]
    
    let total_usage = keys.reduce(fn(acc, k) { acc + k.usage_count }, 0)
    let total_capacity = keys.reduce(fn(acc, k) { acc + k.max_usage }, 0)
    let overall_usage_rate = if total_capacity > 0 {
      (total_usage.to_float() / total_capacity.to_float()) * 100.0
    } else {
      0.0
    }
    
    let successful_operations = operations.filter(fn(op) { op.success }).length()
    let total_operations = operations.length()
    let operation_success_rate = if total_operations > 0 {
      (successful_operations.to_float() / total_operations.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      total_keys,
      keys_by_status,
      keys_by_algorithm,
      total_usage,
      total_capacity,
      overall_usage_rate,
      successful_operations,
      total_operations,
      operation_success_rate
    }
  }
  
  let all_operations = [
    encryption_result,
    decryption_result,
    expired_encryption_result,
    wrong_key_decryption
  ]
  
  let key_metrics = calculate_key_metrics(updated_keys, all_operations)
  assert_eq(key_metrics.total_keys, 3)
  assert_eq(key_metrics.keys_by_status.filter(fn(s) { s.0 == "active" })[0].1, 2)
  assert_eq(key_metrics.keys_by_status.filter(fn(s) { s.0 == "expired" })[0].1, 1)
  assert_eq(key_metrics.keys_by_algorithm.filter(fn(a) { a.0 == "AES-256-GCM" })[0].1, 2)
  assert_eq(key_metrics.keys_by_algorithm.filter(fn(a) { a.0 == "RSA-2048" })[0].1, 1)
  assert_eq(key_metrics.total_usage, 8051)  // 100 + 1 + 50 + 8000
  assert_eq(key_metrics.total_capacity, 25000)  // 10000 + 5000 + 10000
  assert_eq(key_metrics.successful_operations, 2)
  assert_eq(key_metrics.total_operations, 4)
  assert_eq(key_metrics.operation_success_rate, 50.0)
}