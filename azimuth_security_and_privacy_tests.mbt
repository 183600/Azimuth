// Azimuth 安全性测试
// 专注于测试数据加密、隐私保护和安全遥测功能

// 测试1: 敏感数据加密和脱敏
test "敏感数据加密和脱敏测试" {
  // 创建安全管理器
  let security_manager = @azimuth.security.SecurityManager.create({
    "encryption.key.source": "environment",
    "encryption.algorithm": "AES-256-GCM",
    "data.masking.enabled": true
  })
  
  // 验证安全管理器创建成功
  assert_true(@azimuth.security.SecurityManager.is_valid(security_manager))
  
  // 创建包含敏感数据的遥测数据
  let sensitive_data = {
    "user.id": "12345678-1234-1234-1234-123456789012",
    "user.email": "user@example.com",
    "user.phone": "+1-555-123-4567",
    "credit.card": "4111-1111-1111-1111",
    "ssn": "123-45-6789",
    "api.key": "sk-1234567890abcdef",
    "password": "SuperSecretPassword123!",
    "public.info": "This is public information"
  }
  
  // 应用数据脱敏
  let masked_data = security_manager.mask_sensitive_attributes(sensitive_data)
  
  // 验证敏感数据已被脱敏
  assert_eq(masked_data["user.id"], "****-****-****-****-************")
  assert_eq(masked_data["user.email"], "****@example.com")
  assert_eq(masked_data["user.phone"], "+*-***-***-****")
  assert_eq(masked_data["credit.card"], "****-****-****-1111")
  assert_eq(masked_data["ssn"], "***-**-****")
  assert_eq(masked_data["api.key"], "sk-****************")
  assert_eq(masked_data["password"], "********************")
  
  // 验证公开信息未被修改
  assert_eq(masked_data["public.info"], "This is public information")
}

// 测试2: 安全的上下文传播
test "安全的上下文传播测试" {
  // 创建安全传播器
  let secure_propagator = @azimuth.security.SecurePropagator.create({
    "signing.enabled": true,
    "encryption.enabled": true,
    "signature.algorithm": "HMAC-SHA256",
    "key.rotation.enabled": true
  })
  
  // 验证传播器创建成功
  assert_true(@azimuth.security.SecurePropagator.is_valid(secure_propagator))
  
  // 创建原始上下文
  let original_context = @azimuth.telemetry.Context.create()
    .set_trace_id(@azimuth.trace.generate_id())
    .set_span_id(@azimuth.trace.generate_span_id())
    .set_attribute("user.id", "secure-user-123")
    .set_attribute("security.level", "high")
  
  // 注射到载体
  let carrier = {}
  let inject_result = secure_propagator.inject(original_context, carrier)
  
  // 验证注射成功
  assert_true(inject_result.success)
  assert_true(carrier.has_key("traceparent"))
  assert_true(carrier.has_key("tracestate"))
  assert_true(carrier.has_key("azimuth-signature"))
  assert_true(carrier.has_key("azimuth-encrypted"))
  
  // 从载体提取上下文
  let extract_result = secure_propagator.extract(carrier)
  
  // 验证提取成功
  assert_true(extract_result.success)
  assert_true(extract_result.verified)  // 签名验证成功
  assert_eq(extract_result.context.trace_id, original_context.trace_id)
  assert_eq(extract_result.context.span_id, original_context.span_id)
  assert_eq(extract_result.context.attributes["user.id"], "secure-user-123")
  assert_eq(extract_result.context.attributes["security.level"], "high")
}

// 测试3: 访问控制和权限验证
test "访问控制和权限验证测试" {
  // 创建访问控制管理器
  let access_manager = @azimuth.security.AccessManager.create({
    "policy.engine": "RBAC",
    "default.deny": true,
    "audit.enabled": true
  })
  
  // 验证访问管理器创建成功
  assert_true(@azimuth.security.AccessManager.is_valid(access_manager))
  
  // 定义角色和权限
  access_manager.define_role("admin", {
    "telemetry.read": true,
    "telemetry.write": true,
    "telemetry.admin": true,
    "sensitive.data.access": true
  })
  
  access_manager.define_role("operator", {
    "telemetry.read": true,
    "telemetry.write": true,
    "telemetry.admin": false,
    "sensitive.data.access": false
  })
  
  access_manager.define_role("viewer", {
    "telemetry.read": true,
    "telemetry.write": false,
    "telemetry.admin": false,
    "sensitive.data.access": false
  })
  
  // 创建用户并分配角色
  let admin_user = access_manager.create_user("admin-001", ["admin"])
  let operator_user = access_manager.create_user("operator-001", ["operator"])
  let viewer_user = access_manager.create_user("viewer-001", ["viewer"])
  
  // 测试管理员权限
  assert_true(access_manager.check_permission(admin_user, "telemetry.read"))
  assert_true(access_manager.check_permission(admin_user, "telemetry.write"))
  assert_true(access_manager.check_permission(admin_user, "telemetry.admin"))
  assert_true(access_manager.check_permission(admin_user, "sensitive.data.access"))
  
  // 测试操作员权限
  assert_true(access_manager.check_permission(operator_user, "telemetry.read"))
  assert_true(access_manager.check_permission(operator_user, "telemetry.write"))
  assert_false(access_manager.check_permission(operator_user, "telemetry.admin"))
  assert_false(access_manager.check_permission(operator_user, "sensitive.data.access"))
  
  // 测试查看者权限
  assert_true(access_manager.check_permission(viewer_user, "telemetry.read"))
  assert_false(access_manager.check_permission(viewer_user, "telemetry.write"))
  assert_false(access_manager.check_permission(viewer_user, "telemetry.admin"))
  assert_false(access_manager.check_permission(viewer_user, "sensitive.data.access"))
  
  // 测试审计日志
  let audit_logs = access_manager.get_audit_logs()
  assert_true(audit_logs.length() > 0)
  
  // 验证审计日志包含权限检查记录
  let permission_checks = audit_logs.filter(log => log.action == "permission_check")
  assert_true(permission_checks.length() > 0)
}

// 测试4: 遥测数据完整性验证
test "遥测数据完整性验证测试" {
  // 创建数据完整性验证器
  let integrity_validator = @azimuth.security.IntegrityValidator.create({
    "hash.algorithm": "SHA-256",
    "signature.algorithm": "RSA-2048",
    "timestamp.validation": true
  })
  
  // 验证完整性验证器创建成功
  assert_true(@azimuth.security.IntegrityValidator.is_valid(integrity_validator))
  
  // 创建遥测数据
  let telemetry_data = {
    "trace_id": @azimuth.trace.generate_id(),
    "span_id": @azimuth.trace.generate_span_id(),
    "operation.name": "secure.operation",
    "start_time": @azimuth.time.now(),
    "duration.ms": 150,
    "status": "success",
    "attributes": {
      "user.id": "user-123",
      "service.name": "auth-service",
      "region": "us-east-1"
    }
  }
  
  // 计算数据签名
  let signature = integrity_validator.sign_data(telemetry_data)
  
  // 验证签名不为空
  assert_true(signature.length() > 0)
  
  // 验证数据完整性
  let is_valid = integrity_validator.verify_data(telemetry_data, signature)
  assert_true(is_valid)
  
  // 测试篡改检测
  let tampered_data = telemetry_data
  tampered_data["duration.ms"] = 200  // 篡改数据
  
  // 验证篡改后的数据完整性检查失败
  let is_tampered_valid = integrity_validator.verify_data(tampered_data, signature)
  assert_false(is_tampered_valid)
}

// 测试5: 安全日志和威胁检测
test "安全日志和威胁检测测试" {
  // 创建安全事件检测器
  let threat_detector = @azimuth.security.ThreatDetector.create({
    "detection.rules": ["brute_force", "data_exfiltration", "unusual_access"],
    "alert.threshold": 3,
    "time.window.ms": 60000  // 1分钟窗口
  })
  
  // 验证威胁检测器创建成功
  assert_true(@azimuth.security.ThreatDetector.is_valid(threat_detector))
  
  // 模拟正常访问事件
  for i = 0; i < 5; i = i + 1 {
    let event = {
      "timestamp": @azimuth.time.now(),
      "event.type": "authentication",
      "user.id": "user-" + @azimuth.string.from_int(i),
      "source.ip": "192.168.1.100",
      "result": "success",
      "user.agent": "Mozilla/5.0 (compatible; Bot/1.0)"
    }
    threat_detector.process_event(event)
  }
  
  // 检查是否有威胁
  let threats_after_normal = threat_detector.get_detected_threats()
  assert_eq(threats_after_normal.length(), 0)  // 正常访问不应触发威胁
  
  // 模拟暴力破解攻击
  let attacker_ip = "10.0.0.50"
  for i = 0; i < 10; i = i + 1 {
    let event = {
      "timestamp": @azimuth.time.now(),
      "event.type": "authentication",
      "user.id": "admin",  // 尝试登录管理员账户
      "source.ip": attacker_ip,
      "result": i < 8 ? "failed" : "success",  // 前8次失败，第9次成功
      "user.agent": "BruteForceBot/1.0"
    }
    threat_detector.process_event(event)
  }
  
  // 检查是否检测到暴力破解攻击
  let threats_after_attack = threat_detector.get_detected_threats()
  assert_true(threats_after_attack.length() > 0)
  
  // 验证威胁详情
  let brute_force_threat = threats_after_attack.find(threat => threat.type == "brute_force")
  assert_true(brute_force_threat != null)
  assert_eq(brute_force_threat.source_ip, attacker_ip)
  assert_true(brute_force_threat.severity >= 7)  // 高严重性
  assert_true(brute_force_threat.failed_attempts >= 8)
  
  // 模拟数据外泄尝试
  let exfiltration_events = [
    {"data.volume": 1024, "destination": "unknown-external.com"},
    {"data.volume": 2048, "destination": "unknown-external.com"},
    {"data.volume": 4096, "destination": "unknown-external.com"},
    {"data.volume": 8192, "destination": "unknown-external.com"}
  ]
  
  for event_data in exfiltration_events {
    let event = {
      "timestamp": @azimuth.time.now(),
      "event.type": "data.transfer",
      "user.id": "insider-user",
      "source.ip": "192.168.1.200",
      "destination": event_data.destination,
      "data.volume": event_data.data.volume,
      "classification": "confidential"
    }
    threat_detector.process_event(event)
  }
  
  // 检查是否检测到数据外泄
  let final_threats = threat_detector.get_detected_threats()
  let exfiltration_threat = final_threats.find(threat => threat.type == "data_exfiltration")
  assert_true(exfiltration_threat != null)
  assert_eq(exfiltration_threat.user_id, "insider-user")
  assert_true(exfiltration_threat.total_data_volume > 10000)  // 超过10KB
}