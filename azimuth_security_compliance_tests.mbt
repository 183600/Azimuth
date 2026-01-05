// Azimuth Security Compliance Test Suite
// 安全合规测试套件 - 测试遥测系统的安全性和合规性功能

// Test 1: 数据加密和脱敏
test "data encryption and anonymization" {
  // 创建安全管理器
  let security_manager = @azimuth.SecurityManager::new(
    @azimuth.SecurityConfig {
      encryption_enabled: true,
      encryption_algorithm: "aes-256-gcm",
      key_rotation_interval_hours: 24,
      anonymization_enabled: true,
      pii_fields: ["user.id", "user.email", "user.phone", "ip.address"]
    }
  )
  
  // 创建包含敏感信息的遥测数据
  let sensitive_telemetry = @azimuth.TelemetryData {
    trace_id: "secure-trace-123456",
    span_id: "secure-span-789012",
    operation_name: "user.profile.update",
    service_name: "user-service",
    duration_ms: 150,
    status: "success",
    attributes: [
      ("user.id", @azimuth.StringValue("user-12345-abcd")),
      ("user.email", @azimuth.StringValue("user@example.com")),
      ("user.phone", @azimuth.StringValue("+1-555-123-4567")),
      ("ip.address", @azimuth.StringValue("192.168.1.100")),
      ("user.session", @azimuth.StringValue("sess-abcdef123456")),
      ("request.id", @azimuth.StringValue("req-789012345678")),
      ("non.sensitive.data", @azimuth.StringValue("this-is-safe"))
    ]
  }
  
  // 执行数据脱敏
  let anonymized_data = @azimuth.SecurityManager::anonymize(security_manager, sensitive_telemetry)
  
  // 验证PII字段已脱敏
  let anonymized_attrs = anonymized_data.attributes
  
  // 验证用户ID已脱敏
  match @azimuth.Attributes::get(anonymized_attrs, "user.id") {
    Some(@azimuth.StringValue(user_id)) => {
      assert_false(user_id.contains("user-12345-abcd"))
      assert_true(user_id.contains("*****"))
      assert_eq(user_id.length(), "user-12345-abcd".length())  // 保持长度一致
    }
    _ => assert_true(false)
  }
  
  // 验证邮箱已脱敏
  match @azimuth.Attributes::get(anonymized_attrs, "user.email") {
    Some(@azimuth.StringValue(email)) => {
      assert_false(email.contains("user@example.com"))
      assert_true(email.contains("*****"))
      assert_true(email.contains("@"))  // 保留邮箱格式
    }
    _ => assert_true(false)
  }
  
  // 验证电话号码已脱敏
  match @azimuth.Attributes::get(anonymized_attrs, "user.phone") {
    Some(@azimuth.StringValue(phone)) => {
      assert_false(phone.contains("+1-555-123-4567"))
      assert_true(phone.contains("*****"))
    }
    _ => assert_true(false)
  }
  
  // 验证IP地址已脱敏
  match @azimuth.Attributes::get(anonymized_attrs, "ip.address") {
    Some(@azimuth.StringValue(ip)) => {
      assert_false(ip.contains("192.168.1.100"))
      assert_true(ip.contains("*****"))
    }
    _ => assert_true(false)
  }
  
  // 验证非敏感数据未受影响
  match @azimuth.Attributes::get(anonymized_attrs, "non.sensitive.data") {
    Some(@azimuth.StringValue(data)) => assert_eq(data, "this-is-safe")
    _ => assert_true(false)
  }
  
  // 验证会话ID已脱敏 (虽然不在PII列表中，但可能包含敏感信息)
  match @azimuth.Attributes::get(anonymized_attrs, "user.session") {
    Some(@azimuth.StringValue(session)) => {
      assert_false(session.contains("sess-abcdef123456"))
      assert_true(session.contains("*****"))
    }
    _ => assert_true(false)
  }
  
  // 执行数据加密
  let encrypted_data = @azimuth.SecurityManager::encrypt(security_manager, anonymized_data)
  
  // 验证数据已加密
  assert_true(encrypted_data.encrypted)
  assert_true(encrypted_data.encrypted_data.length() > 0)
  assert_false(encrypted_data.encrypted_data.contains("user.id"))
  assert_false(encrypted_data.encrypted_data.contains("*****"))
  
  // 验证加密元数据
  assert_eq(encrypted_data.encryption_algorithm, "aes-256-gcm")
  assert_true(encrypted_data.encryption_key_id.length() > 0)
  assert_true(encrypted_data.encryption_timestamp > 0)
  
  // 执行数据解密
  let decrypted_data = @azimuth.SecurityManager::decrypt(security_manager, encrypted_data)
  
  // 验证解密结果与脱敏数据一致
  assert_eq(decrypted_data.trace_id, anonymized_data.trace_id)
  assert_eq(decrypted_data.span_id, anonymized_data.span_id)
  assert_eq(decrypted_data.operation_name, anonymized_data.operation_name)
  assert_eq(decrypted_data.attributes.length(), anonymized_data.attributes.length())
}

// Test 2: 访问控制和权限管理
test "access control and permission management" {
  // 创建访问控制管理器
  let access_manager = @azimuth.AccessControlManager::new(
    @azimuth.AccessControlConfig {
      authentication_required: true,
      authorization_enabled: true,
      role_based_access: true,
      default_permission: "deny"
    }
  )
  
  // 定义角色和权限
  let admin_role = @azimuth.Role::new("admin", [
    "telemetry.read",
    "telemetry.write",
    "telemetry.delete",
    "config.read",
    "config.write",
    "user.manage"
  ])
  
  let analyst_role = @azimuth.Role::new("analyst", [
    "telemetry.read",
    "config.read"
  ])
  
  let operator_role = @azimuth.Role::new("operator", [
    "telemetry.read",
    "telemetry.write",
    "config.read"
  ])
  
  let viewer_role = @azimuth.Role::new("viewer", [
    "telemetry.read"
  ])
  
  // 添加角色到访问控制管理器
  @azimuth.AccessControlManager::add_role(access_manager, admin_role)
  @azimuth.AccessControlManager::add_role(access_manager, analyst_role)
  @azimuth.AccessControlManager::add_role(access_manager, operator_role)
  @azimuth.AccessControlManager::add_role(access_manager, viewer_role)
  
  // 创建用户
  let admin_user = @azimuth.User::new("admin-user", ["admin"])
  let analyst_user = @azimuth.User::new("analyst-user", ["analyst"])
  let operator_user = @azimuth.User::new("operator-user", ["operator"])
  let viewer_user = @azimuth.User::new("viewer-user", ["viewer"])
  
  // 添加用户到访问控制管理器
  @azimuth.AccessControlManager::add_user(access_manager, admin_user)
  @azimuth.AccessControlManager::add_user(access_manager, analyst_user)
  @azimuth.AccessControlManager::add_user(access_manager, operator_user)
  @azimuth.AccessControlManager::add_user(access_manager, viewer_user)
  
  // 测试管理员权限
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "telemetry.read"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "telemetry.write"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "telemetry.delete"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "config.read"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "config.write"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "admin-user", "user.manage"))
  
  // 测试分析师权限
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "telemetry.read"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "telemetry.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "telemetry.delete"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "config.read"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "config.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "analyst-user", "user.manage"))
  
  // 测试操作员权限
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "telemetry.read"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "telemetry.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "telemetry.delete"))
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "config.read"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "config.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "operator-user", "user.manage"))
  
  // 测试查看者权限
  assert_true(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "telemetry.read"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "telemetry.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "telemetry.delete"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "config.read"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "config.write"))
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "viewer-user", "user.manage"))
  
  // 测试不存在的用户
  assert_false(@azimuth.AccessControlManager::check_permission(access_manager, "nonexistent-user", "telemetry.read"))
  
  // 测试资源级访问控制
  let resource_a = @azimuth.SecureResource::new("resource-a", "service-a", ["admin", "operator"])
  let resource_b = @azimuth.SecureResource::new("resource-b", "service-b", ["admin", "analyst"])
  
  @azimuth.AccessControlManager::add_resource(access_manager, resource_a)
  @azimuth.AccessControlManager::add_resource(access_manager, resource_b)
  
  // 测试资源级权限
  assert_true(@azimuth.AccessControlManager::check_resource_access(access_manager, "admin-user", "resource-a"))
  assert_true(@azimuth.AccessControlManager::check_resource_access(access_manager, "admin-user", "resource-b"))
  
  assert_true(@azimuth.AccessControlManager::check_resource_access(access_manager, "operator-user", "resource-a"))
  assert_false(@azimuth.AccessControlManager::check_resource_access(access_manager, "operator-user", "resource-b"))
  
  assert_false(@azimuth.AccessControlManager::check_resource_access(access_manager, "analyst-user", "resource-a"))
  assert_true(@azimuth.AccessControlManager::check_resource_access(access_manager, "analyst-user", "resource-b"))
  
  assert_false(@azimuth.AccessControlManager::check_resource_access(access_manager, "viewer-user", "resource-a"))
  assert_false(@azimuth.AccessControlManager::check_resource_access(access_manager, "viewer-user", "resource-b"))
}

// Test 3: 审计日志和合规性报告
test "audit logging and compliance reporting" {
  // 创建审计日志管理器
  let audit_manager = @azimuth.AuditManager::new(
    @azimuth.AuditConfig {
      log_all_access: true,
      log_data_changes: true,
      log_config_changes: true,
      log_authentication: true,
      log_authorization: true,
      retention_days: 90,
      enable_compliance_reporting: true
    }
  )
  
  // 模拟审计事件
  let authentication_event = @azimuth.AuditEvent::new(
    "authentication.success",
    "user-login",
    "analyst-user",
    @azimuth.current_timestamp(),
    [
      ("source.ip", @azimuth.StringValue("192.168.1.50")),
      ("user.agent", @azimuth.StringValue("Mozilla/5.0")),
      ("login.method", @azimuth.StringValue("password"))
    ]
  )
  
  let authorization_event = @azimuth.AuditEvent::new(
    "authorization.denied",
    "telemetry.delete",
    "viewer-user",
    @azimuth.current_timestamp(),
    [
      ("resource.id", @azimuth.StringValue("trace-123456")),
      ("permission.required", @azimuth.StringValue("telemetry.delete")),
      ("reason", @azimuth.StringValue("insufficient_privileges"))
    ]
  )
  
  let data_access_event = @azimuth.AuditEvent::new(
    "data.access",
    "telemetry.read",
    "analyst-user",
    @azimuth.current_timestamp(),
    [
      ("query.type", @azimuth.StringValue("time_range")),
      ("time.start", @azimuth.StringValue("2023-01-01T00:00:00Z")),
      ("time.end", @azimuth.StringValue("2023-01-02T00:00:00Z")),
      ("record.count", @azimuth.IntValue(1500))
    ]
  )
  
  let config_change_event = @azimuth.AuditEvent::new(
    "config.change",
    "telemetry.config.update",
    "admin-user",
    @azimuth.current_timestamp(),
    [
      ("config.key", @azimuth.StringValue("sampling.rate")),
      ("old.value", @azimuth.StringValue("0.1")),
      ("new.value", @azimuth.StringValue("0.2")),
      ("change.reason", @azimuth.StringValue("performance_optimization"))
    ]
  )
  
  // 记录审计事件
  @azimuth.AuditManager::log_event(audit_manager, authentication_event)
  @azimuth.AuditManager::log_event(audit_manager, authorization_event)
  @azimuth.AuditManager::log_event(audit_manager, data_access_event)
  @azimuth.AuditManager::log_event(audit_manager, config_change_event)
  
  // 查询审计日志
  let all_events = @azimuth.AuditManager::get_all_events(audit_manager)
  assert_eq(all_events.length(), 4)
  
  // 按事件类型查询
  let auth_events = @azimuth.AuditManager::get_events_by_type(audit_manager, "authentication")
  assert_eq(auth_events.length(), 1)
  assert_eq(auth_events[0].event_type, "authentication.success")
  
  let access_events = @azimuth.AuditManager::get_events_by_type(audit_manager, "authorization")
  assert_eq(access_events.length(), 1)
  assert_eq(access_events[0].event_type, "authorization.denied")
  
  // 按用户查询
  let analyst_events = @azimuth.AuditManager::get_events_by_user(audit_manager, "analyst-user")
  assert_eq(analyst_events.length(), 2)
  assert_true(analyst_events.map(fn(e) { e.event_type }).contains("authentication.success"))
  assert_true(analyst_events.map(fn(e) { e.event_type }).contains("data.access"))
  
  // 按时间范围查询
  let current_time = @azimuth.current_timestamp()
  let time_filtered_events = @azimuth.AuditManager::get_events_by_time_range(
    audit_manager,
    current_time - 3600000,  // 1小时前
    current_time + 3600000   // 1小时后
  )
  assert_eq(time_filtered_events.length(), 4)  // 所有事件都在时间范围内
  
  // 生成合规性报告
  let compliance_report = @azimuth.AuditManager::generate_compliance_report(
    audit_manager,
    current_time - 86400000,  // 24小时前
    current_time
  )
  
  // 验证合规性报告内容
  assert_eq(compliance_report.report_period_start_ms, current_time - 86400000)
  assert_eq(compliance_report.report_period_end_ms, current_time)
  assert_eq(compliance_report.total_events, 4)
  
  // 验证事件类型统计
  assert_eq(compliance_report.event_type_counts.get("authentication"), Some(1))
  assert_eq(compliance_report.event_type_counts.get("authorization"), Some(1))
  assert_eq(compliance_report.event_type_counts.get("data"), Some(1))
  assert_eq(compliance_report.event_type_counts.get("config"), Some(1))
  
  // 验证用户活动统计
  assert_eq(compliance_report.user_activity_counts.get("analyst-user"), Some(2))
  assert_eq(compliance_report.user_activity_counts.get("viewer-user"), Some(1))
  assert_eq(compliance_report.user_activity_counts.get("admin-user"), Some(1))
  
  // 验证安全事件统计
  assert_eq(compliance_report.security_events, 1)  // 只有authorization.denied
  
  // 验证合规性检查
  assert_true(compliance_report.all_events_logged)
  assert_true(compliance_report.sufficient_retention_period)
  assert_true(compliance_report.required_fields_present)
  
  // 导出审计日志
  let export_result = @azimuth.AuditManager::export_logs(
    audit_manager,
    current_time - 86400000,
    current_time,
    "json"
  )
  
  assert_true(export_result.success)
  assert_true(export_result.export_size_bytes > 0)
  assert_eq(export_result.exported_events_count, 4)
  assert_eq(export_result.export_format, "json")
  assert_true(export_result.export_data.contains("authentication.success"))
  assert_true(export_result.export_data.contains("authorization.denied"))
}

// Test 4: 数据保留和清理策略
test "data retention and cleanup policies" {
  // 创建数据保留管理器
  let retention_manager = @azimuth.DataRetentionManager::new(
    @azimuth.RetentionConfig {
      default_retention_days: 30,
      max_retention_days: 365,
      retention_policies: [
        @azimuth.RetentionPolicy {
          data_type: "trace",
          retention_days: 7,
          archival_enabled: true,
          archival_days: 365
        },
        @azimuth.RetentionPolicy {
          data_type: "metric",
          retention_days: 30,
          archival_enabled: false,
          archival_days: 0
        },
        @azimuth.RetentionPolicy {
          data_type: "log",
          retention_days: 90,
          archival_enabled: true,
          archival_days: 2555  // 7年
        },
        @azimuth.RetentionPolicy {
          data_type: "audit",
          retention_days: 2555,  // 7年
          archival_enabled: false,
          archival_days: 0
        }
      ],
      cleanup_interval_hours: 24,
      secure_deletion: true
    }
  )
  
  // 创建不同类型的测试数据
  let current_time = @azimuth.current_timestamp()
  let day_in_ms = 86400000
  
  // 创建7天前的跟踪数据 (应该保留)
  let recent_trace_data = @azimuth.TelemetryDataRecord {
    id: "trace-recent",
    data_type: "trace",
    timestamp: current_time - 7 * day_in_ms,
    data: "recent trace data",
    size_bytes: 1024
  }
  
  // 创建10天前的跟踪数据 (应该被归档)
  let old_trace_data = @azimuth.TelemetryDataRecord {
    id: "trace-old",
    data_type: "trace",
    timestamp: current_time - 10 * day_in_ms,
    data: "old trace data",
    size_bytes: 1024
  }
  
  // 创建30天前的指标数据 (应该保留)
  let recent_metric_data = @azimuth.TelemetryDataRecord {
    id: "metric-recent",
    data_type: "metric",
    timestamp: current_time - 30 * day_in_ms,
    data: "recent metric data",
    size_bytes: 512
  }
  
  // 创建40天前的指标数据 (应该被删除)
  let old_metric_data = @azimuth.TelemetryDataRecord {
    id: "metric-old",
    data_type: "metric",
    timestamp: current_time - 40 * day_in_ms,
    data: "old metric data",
    size_bytes: 512
  }
  
  // 创建90天前的日志数据 (应该保留)
  let recent_log_data = @azimuth.TelemetryDataRecord {
    id: "log-recent",
    data_type: "log",
    timestamp: current_time - 90 * day_in_ms,
    data: "recent log data",
    size_bytes: 2048
  }
  
  // 创建100天前的日志数据 (应该被归档)
  let old_log_data = @azimuth.TelemetryDataRecord {
    id: "log-old",
    data_type: "log",
    timestamp: current_time - 100 * day_in_ms,
    data: "old log data",
    size_bytes: 2048
  }
  
  // 添加数据到存储
  @azimuth.DataRetentionManager::add_data(retention_manager, recent_trace_data)
  @azimuth.DataRetentionManager::add_data(retention_manager, old_trace_data)
  @azimuth.DataRetentionManager::add_data(retention_manager, recent_metric_data)
  @azimuth.DataRetentionManager::add_data(retention_manager, old_metric_data)
  @azimuth.DataRetentionManager::add_data(retention_manager, recent_log_data)
  @azimuth.DataRetentionManager::add_data(retention_manager, old_log_data)
  
  // 执行数据保留检查
  let retention_analysis = @azimuth.DataRetentionManager::analyze_retention(retention_manager)
  
  // 验证保留分析结果
  assert_eq(retention_analysis.total_records, 6)
  assert_eq(retention_analysis.expired_count, 2)  // old_metric_data 和 old_log_data
  assert_eq(retention_analysis.archivable_count, 2)  // old_trace_data 和 old_log_data
  assert_eq(retention_analysis.active_count, 2)  // recent_trace_data 和 recent_metric_data
  assert_eq(retention_analysis.archived_count, 0)  // 初始状态，没有已归档数据
  
  // 执行数据清理
  let cleanup_result = @azimuth.DataRetentionManager::execute_cleanup(retention_manager)
  
  // 验证清理结果
  assert_true(cleanup_result.success)
  assert_eq(cleanup_result.deleted_count, 1)  // old_metric_data 应该被删除
  assert_eq(cleanup_result.archived_count, 2)  // old_trace_data 和 old_log_data 应该被归档
  assert_eq(cleanup_result.errors.length(), 0)
  
  // 验证归档数据
  let archived_data = @azimuth.DataRetentionManager::get_archived_data(retention_manager)
  assert_eq(archived_data.length(), 2)
  
  let trace_archive = archived_data.find(fn(record) { record.id == "trace-old" })
  match trace_archive {
    Some(record) => {
      assert_eq(record.data_type, "trace")
      assert_eq(record.archived, true)
      assert_true(record.archive_timestamp > 0)
    }
    None => assert_true(false)
  }
  
  let log_archive = archived_data.find(fn(record) { record.id == "log-old" })
  match log_archive {
    Some(record) => {
      assert_eq(record.data_type, "log")
      assert_eq(record.archived, true)
      assert_true(record.archive_timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // 验证剩余活跃数据
  let active_data = @azimuth.DataRetentionManager::get_active_data(retention_manager)
  assert_eq(active_data.length(), 3)  // recent_trace_data, recent_metric_data, recent_log_data
  
  // 测试安全删除
  let secure_delete_data = @azimuth.TelemetryDataRecord {
    id: "to-delete",
    data_type: "metric",
    timestamp: current_time - 100 * day_in_ms,  // 100天前，应该被删除
    data: "sensitive data to delete securely",
    size_bytes: 1024
  }
  
  @azimuth.DataRetentionManager::add_data(retention_manager, secure_delete_data)
  
  // 执行安全删除
  let secure_delete_result = @azimuth.DataRetentionManager::secure_delete(retention_manager, "to-delete")
  assert_true(secure_delete_result.success)
  assert_eq(secure_delete_result.deleted_id, "to-delete")
  assert_true(secure_delete_result.secure_deletion_used)
  
  // 验证数据已被安全删除
  let deleted_data = @azimuth.DataRetentionManager::get_data_by_id(retention_manager, "to-delete")
  assert_eq(deleted_data, None)
}

// Test 5: GDPR合规性和数据主体权利
test "gdpr compliance and data subject rights" {
  // 创建GDPR合规管理器
  let gdpr_manager = @azimuth.GDPRManager::new(
    @azimuth.GDPRConfig {
      lawful_basis: "legitimate_interest",
      data_controller: "Azimuth Corporation",
      data_protection_officer: "dpo@azimuth.com",
      retention_purpose: "service_improvement",
      data_subject_rights_enabled: true,
      consent_management: true,
      breach_notification_enabled: true
    }
  )
  
  // 创建数据主体记录
  let data_subject = @azimuth.DataSubject::new(
    "user-12345",
    "user@example.com",
    [
      ("name", "John Doe"),
      ("address", "123 Main St, Anytown, USA"),
      ("phone", "+1-555-123-4567")
    ]
  )
  
  // 记录数据主体同意
  let consent_record = @azimuth.ConsentRecord::new(
    "user-12345",
    "telemetry_processing",
    true,
    @azimuth.current_timestamp(),
    [
      ("purpose", "service_improvement"),
      ("data_types", "usage_metrics,performance_data"),
      ("retention_period", "365_days"),
      ("withdrawal_method", "email_privacy@azimuth.com")
    ]
  )
  
  @azimuth.GDPRManager::register_data_subject(gdpr_manager, data_subject)
  @azimuth.GDPRManager::record_consent(gdpr_manager, consent_record)
  
  // 创建与数据主体相关的遥测数据
  let user_telemetry_data = [
    @azimuth.TelemetryDataRecord {
      id: "telemetry-1",
      data_type: "trace",
      timestamp: @azimuth.current_timestamp() - 86400000,  // 1天前
      data: "trace data with user-12345",
      size_bytes: 1024,
      subject_id: Some("user-12345")
    },
    @azimuth.TelemetryDataRecord {
      id: "telemetry-2",
      data_type: "metric",
      timestamp: @azimuth.current_timestamp() - 172800000,  // 2天前
      data: "metric data with user-12345",
      size_bytes: 512,
      subject_id: Some("user-12345")
    },
    @azimuth.TelemetryDataRecord {
      id: "telemetry-3",
      data_type: "log",
      timestamp: @azimuth.current_timestamp() - 259200000,  // 3天前
      data: "log data with user-12345",
      size_bytes: 2048,
      subject_id: Some("user-12345")
    }
  ]
  
  // 添加数据到GDPR管理器
  for data in user_telemetry_data {
    @azimuth.GDPRManager::add_personal_data(gdpr_manager, data)
  }
  
  // 测试数据访问权 (DSAR - Data Subject Access Request)
  let access_request = @azimuth.DataSubjectRequest::new(
    "user-12345",
    "access",
    @azimuth.current_timestamp(),
    "Customer requesting access to their personal data"
  )
  
  let access_result = @azimuth.GDPRManager::process_access_request(gdpr_manager, access_request)
  
  // 验证访问请求结果
  assert_true(access_result.success)
  assert_eq(access_result.request_id, access_request.id)
  assert_eq(access_result.subject_id, "user-12345")
  assert_eq(access_result.request_type, "access")
  assert_eq(access_result.data_records.length(), 3)
  
  // 验证返回的数据记录
  let returned_ids = access_result.data_records.map(fn(record) { record.id })
  assert_true(returned_ids.contains("telemetry-1"))
  assert_true(returned_ids.contains("telemetry-2"))
  assert_true(returned_ids.contains("telemetry-3"))
  
  // 验证数据已脱敏
  for record in access_result.data_records {
    assert_true(record.anonymized)
    assert_false(record.data.contains("user-12345"))
  }
  
  // 测试数据修正权
  let rectification_request = @azimuth.DataSubjectRequest::new(
    "user-12345",
    "rectification",
    @azimuth.current_timestamp(),
    "Customer requesting correction of email address"
  )
  
  let rectification_result = @azimuth.GDPRManager::process_rectification_request(
    gdpr_manager,
    rectification_request,
    [
      ("email", "john.doe@newemail.com")
    ]
  )
  
  // 验证修正请求结果
  assert_true(rectification_result.success)
  assert_eq(rectification_result.updated_fields.length(), 1)
  assert_eq(rectification_result.updated_fields[0], "email")
  
  // 验证数据主体信息已更新
  let updated_subject = @azimuth.GDPRManager::get_data_subject(gdpr_manager, "user-12345")
  match updated_subject {
    Some(subject) => {
      assert_eq(subject.attributes.get("email"), Some("john.doe@newemail.com"))
    }
    None => assert_true(false)
  }
  
  // 测试数据删除权 (被遗忘权)
  let erasure_request = @azimuth.DataSubjectRequest::new(
    "user-12345",
    "erasure",
    @azimuth.current_timestamp(),
    "Customer requesting deletion of their personal data"
  )
  
  let erasure_result = @azimuth.GDPRManager::process_erasure_request(gdpr_manager, erasure_request)
  
  // 验证删除请求结果
  assert_true(erasure_result.success)
  assert_eq(erasure_result.deleted_records_count, 3)
  assert_eq(erasure_result.deleted_data_types, ["trace", "metric", "log"])
  
  // 验证数据已被删除
  let remaining_data = @azimuth.GDPRManager::get_personal_data(gdpr_manager, "user-12345")
  assert_eq(remaining_data.length(), 0)
  
  // 测试数据可携带权
  let portability_request = @azimuth.DataSubjectRequest::new(
    "user-67890",  // 不同的用户
    "portability",
    @azimuth.current_timestamp(),
    "Customer requesting their data in a portable format"
  )
  
  // 为新用户创建一些数据
  let new_user_data = @azimuth.TelemetryDataRecord {
    id: "telemetry-new-user",
    data_type: "trace",
    timestamp: @azimuth.current_timestamp() - 86400000,
    data: "trace data with user-67890",
    size_bytes: 1024,
    subject_id: Some("user-67890")
  }
  
  @azimuth.GDPRManager::add_personal_data(gdpr_manager, new_user_data)
  
  let portability_result = @azimuth.GDPRManager::process_portability_request(gdpr_manager, portability_request)
  
  // 验证可携带性请求结果
  assert_true(portability_result.success)
  assert_eq(portability_result.export_format, "json")
  assert_true(portability_result.export_data.contains("user-67890"))
  assert_true(portability_result.export_data.contains("telemetry-new-user"))
  
  // 测试同意撤回
  let consent_withdrawal = @azimuth.ConsentWithdrawal::new(
    "user-67890",
    "telemetry_processing",
    @azimuth.current_timestamp(),
    "User withdrawing consent for telemetry processing"
  )
  
  let withdrawal_result = @azimuth.GDPRManager::process_consent_withdrawal(gdpr_manager, consent_withdrawal)
  
  // 验证同意撤回结果
  assert_true(withdrawal_result.success)
  assert_eq(withdrawal_result.subject_id, "user-67890")
  assert_eq(withdrawal_result.withdrawn_purpose, "telemetry_processing")
  
  // 验证后续数据处理已被阻止
  let new_data_request = @azimuth.DataSubjectRequest::new(
    "user-67890",
    "access",
    @azimuth.current_timestamp(),
    "Checking data after consent withdrawal"
  )
  
  let post_withdrawal_result = @azimuth.GDPRManager::process_access_request(gdpr_manager, new_data_request)
  
  // 根据GDPR，撤回同意后不应再处理新数据
  // 但历史数据可能仍保留（取决于其他法律依据）
  assert_true(post_withdrawal_result.success)
  assert_true(post_withdrawal_result.consent_status.contains("withdrawn"))
}

// Test 6: 安全事件检测和响应
test "security incident detection and response" {
  // 创建安全事件管理器
  let security_manager = @azimuth.SecurityEventManager::new(
    @azimuth.SecurityEventConfig {
      threat_detection_enabled: true,
      anomaly_detection_enabled: true,
      real_time_monitoring: true,
      automated_response: true,
      alert_thresholds: [
        ("failed_login_attempts", 5),
        ("unauthorized_access_attempts", 3),
        ("data_export_volume", 1000000),  // 1MB
        ("unusual_access_patterns", 80.0)  // 80%异常分数
      ]
    }
  )
  
  // 创建安全事件检测器
  let threat_detector = @azimuth.ThreatDetector::new(
    @azimuth.ThreatDetectionConfig {
      detection_rules: [
        @azimuth.DetectionRule {
          name: "brute_force_login",
          condition: "failed_login_count > 5 AND time_window < 300",
          severity: "high",
          response_actions: ["account_lockout", "security_alert"]
        },
        @azimuth.DetectionRule {
          name: "data_exfiltration",
          condition: "data_export_volume > 1000000 AND time_window < 3600",
          severity: "critical",
          response_actions: ["access_revocation", "security_alert", "data_protection_officer_notification"]
        },
        @azimuth.DetectionRule {
          name: "privilege_escalation",
          condition: "unauthorized_privilege_change",
          severity: "high",
          response_actions: ["session_termination", "security_alert"]
        }
      ]
    }
  )
  
  // 模拟安全事件序列
  
  // 1. 模拟暴力登录攻击
  let failed_login_events = []
  let base_time = @azimuth.current_timestamp()
  
  for i = 0; i < 6; i = i + 1 {
    let event = @azimuth.SecurityEvent::new(
      "authentication.failed",
      base_time + i * 30000,  // 每30秒一次失败登录
      [
        ("user.id", @azimuth.StringValue("target-user")),
        ("source.ip", @azimuth.StringValue("192.168.1.100")),
        ("failure.reason", @azimuth.StringValue("invalid_password"))
      ]
    )
    failed_login_events = failed_login_events.push(event)
  }
  
  // 添加失败登录事件到安全事件管理器
  for event in failed_login_events {
    @azimuth.SecurityEventManager::add_event(security_manager, event)
  }
  
  // 检测威胁
  let threat_detection_result = @azimuth.ThreatDetector::detect_threats(threat_detector, failed_login_events)
  
  // 验证威胁检测结果
  assert_eq(threat_detection_result.detected_threats.length(), 1)
  
  let brute_force_threat = threat_detection_result.detected_threats[0]
  assert_eq(brute_force_threat.rule_name, "brute_force_login")
  assert_eq(brute_force_threat.severity, "high")
  assert_true(brute_force_threat.confidence_score > 0.8)
  assert_eq(brute_force_threat.triggered_events.length(), 6)
  
  // 执行自动响应
  let response_result = @azimuth.SecurityEventManager::execute_response(security_manager, brute_force_threat)
  
  // 验证响应结果
  assert_true(response_result.success)
  assert_eq(response_result.executed_actions.length(), 2)
  assert_true(response_result.executed_actions.contains("account_lockout"))
  assert_true(response_result.executed_actions.contains("security_alert"))
  
  // 验证账户已被锁定
  let account_status = @azimuth.SecurityEventManager::get_account_status(security_manager, "target-user")
  assert_eq(account_status.status, "locked")
  assert_true(account_status.lock_reason.contains("brute_force"))
  assert_true(account_status.lock_timestamp > 0)
  
  // 2. 模拟数据外泄事件
  let data_export_events = []
  let export_start_time = base_time + 3600000  // 1小时后
  
  // 模拟大量数据导出
  for i = 0; i < 10; i = i + 1 {
    let event = @azimuth.SecurityEvent::new(
      "data.export",
      export_start_time + i * 300000,  // 每5分钟一次导出
      [
        ("user.id", @azimuth.StringValue("internal-user")),
        ("data.volume", @azimuth.IntValue(150000)),  // 每次导出150KB
        ("data.type", @azimuth.StringValue("telemetry")),
        ("destination", @azimuth.StringValue("external-storage"))
      ]
    )
    data_export_events = data_export_events.push(event)
  }
  
  // 添加数据导出事件到安全事件管理器
  for event in data_export_events {
    @azimuth.SecurityEventManager::add_event(security_manager, event)
  }
  
  // 检测数据外泄威胁
  let exfiltration_detection_result = @azimuth.ThreatDetector::detect_threats(threat_detector, data_export_events)
  
  // 验证数据外泄检测结果
  assert_eq(exfiltration_detection_result.detected_threats.length(), 1)
  
  let exfiltration_threat = exfiltration_detection_result.detected_threats[0]
  assert_eq(exfiltration_threat.rule_name, "data_exfiltration")
  assert_eq(exfiltration_threat.severity, "critical")
  assert_true(exfiltration_threat.confidence_score > 0.9)
  
  // 执行自动响应
  let exfiltration_response_result = @azimuth.SecurityEventManager::execute_response(security_manager, exfiltration_threat)
  
  // 验证响应结果
  assert_true(exfiltration_response_result.success)
  assert_eq(exfiltration_response_result.executed_actions.length(), 3)
  assert_true(exfiltration_response_result.executed_actions.contains("access_revocation"))
  assert_true(exfiltration_response_result.executed_actions.contains("security_alert"))
  assert_true(exfiltration_response_result.executed_actions.contains("data_protection_officer_notification"))
  
  // 验证访问已被撤销
  let user_access = @azimuth.SecurityEventManager::get_user_access(security_manager, "internal-user")
  assert_eq(user_access.status, "revoked")
  assert_true(user_access.revocation_reason.contains("data_exfiltration"))
  
  // 3. 生成安全事件报告
  let security_report = @azimuth.SecurityEventManager::generate_security_report(
    security_manager,
    base_time - 7200000,  // 2小时前
    base_time + 7200000   // 2小时后
  )
  
  // 验证安全报告
  assert_eq(security_report.report_period_start_ms, base_time - 7200000)
  assert_eq(security_report.report_period_end_ms, base_time + 7200000)
  assert_eq(security_report.total_security_events, 16)  // 6个失败登录 + 10个数据导出
  assert_eq(security_report.detected_threats, 2)
  assert_eq(security_report.automated_responses, 2)
  
  // 验证威胁类型统计
  assert_eq(security_report.threat_types.get("brute_force_login"), Some(1))
  assert_eq(security_report.threat_types.get("data_exfiltration"), Some(1))
  
  // 验证严重性统计
  assert_eq(security_report.severity_counts.get("high"), Some(1))
  assert_eq(security_report.severity_counts.get("critical"), Some(1))
  
  // 验证响应动作统计
  assert_eq(security_report.response_actions.get("account_lockout"), Some(1))
  assert_eq(security_report.response_actions.get("security_alert"), Some(2))
  assert_eq(security_report.response_actions.get("access_revocation"), Some(1))
  assert_eq(security_report.response_actions.get("data_protection_officer_notification"), Some(1))
  
  // 验证受影响用户
  assert_eq(security_report.affected_users.length(), 2)
  assert_true(security_report.affected_users.contains("target-user"))
  assert_true(security_report.affected_users.contains("internal-user"))
}