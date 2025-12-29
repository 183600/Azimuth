// 数据隐私保护测试用例
test "pii_detection_and_masking" {
  // 测试个人身份信息(PII)检测和遮蔽
  
  // 1. 定义PII类型
  enum PIIType {
    Email
    PhoneNumber
    SSN  // 社会安全号码
    CreditCard
    IPAddress
    MACAddress
    FullName
    Address
    PassportNumber
    DriverLicense
    BankAccount
    MedicalRecord
    BiometricData
    Geolocation
    DeviceID
  }
  
  // 2. 定义PII检测配置
  struct PIIDetectionConfig {
    enable_email_detection : Bool
    enable_phone_detection : Bool
    enable_ssn_detection : Bool
    enable_credit_card_detection : Bool
    enable_ip_detection : Bool
    enable_address_detection : Bool
    masking_strategy : String  // "full", "partial", "hash", "tokenize"
    preserve_length : Bool
  }
  
  let pii_config = PIIDetectionConfig::{
    enable_email_detection: true,
    enable_phone_detection: true,
    enable_ssn_detection: true,
    enable_credit_card_detection: true,
    enable_ip_detection: true,
    enable_address_detection: true,
    masking_strategy: "partial",
    preserve_length: true
  }
  
  // 3. 定义PII测试样本
  struct PIISample {
    pii_type : PIIType
    original : String
    expected_masked : String
    should_detect : Bool
  }
  
  let pii_samples = [
    // 邮箱地址
    PIISample::{
      pii_type: Email,
      original: "john.doe@example.com",
      expected_masked: "j***.***@e******.com",
      should_detect: true
    },
    PIISample::{
      pii_type: Email,
      original: "user123@company.co.uk",
      expected_masked: "u***123@c******.co.uk",
      should_detect: true
    },
    // 电话号码
    PIISample::{
      pii_type: PhoneNumber,
      original: "+1-555-123-4567",
      expected_masked: "+*-***-***-****",
      should_detect: true
    },
    PIISample::{
      pii_type: PhoneNumber,
      original: "13800138000",
      expected_masked: "138****8000",
      should_detect: true
    },
    // 社会安全号码
    PIISample::{
      pii_type: SSN,
      original: "123-45-6789",
      expected_masked: "***-**-****",
      should_detect: true
    },
    PIISample::{
      pii_type: SSN,
      original: "123456789",
      expected_masked: "*******789",
      should_detect: true
    },
    // 信用卡号
    PIISample::{
      pii_type: CreditCard,
      original: "4111-1111-1111-1111",
      expected_masked: "****-****-****-1111",
      should_detect: true
    },
    PIISample::{
      pii_type: CreditCard,
      original: "5555555555554444",
      expected_masked: "************4444",
      should_detect: true
    },
    // IP地址
    PIISample::{
      pii_type: IPAddress,
      original: "192.168.1.100",
      expected_masked: "192.168.1.***",
      should_detect: true
    },
    PIISample::{
      pii_type: IPAddress,
      original: "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
      expected_masked: "2001:0db8:85a3:****:****:8a2e:****:****",
      should_detect: true
    },
    // MAC地址
    PIISample::{
      pii_type: MACAddress,
      original: "00:1B:44:11:3A:B7",
      expected_masked: "00:1B:44:11:3A:**",
      should_detect: true
    },
    // 全名
    PIISample::{
      pii_type: FullName,
      original: "John Smith",
      expected_masked: "J*** S****",
      should_detect: true
    },
    PIISample::{
      pii_type: FullName,
      original: "张三",
      expected_masked: "张*",
      should_detect: true
    },
    // 地址
    PIISample::{
      pii_type: Address,
      original: "123 Main St, New York, NY 10001",
      expected_masked: "*** *** ***, *** ****, ** *****",
      should_detect: true
    },
    PIISample::{
      pii_type: Address,
      original: "北京市朝阳区建国门外大街1号",
      expected_masked: "北京市朝阳区建国门外大街*号",
      should_detect: true
    }
  ]
  
  // 4. 实现PII检测和遮蔽函数
  fn detect_and_mask_pii(text : String, config : PIIDetectionConfig) -> (String, Array[PIIType]) {
    let mut masked_text = text
    let detected_pii = []
    
    // 邮箱检测
    if config.enable_email_detection and is_email(text) {
      masked_text = mask_email(text, config)
      detected_pii.push(Email)
    }
    
    // 电话号码检测
    if config.enable_phone_detection and is_phone_number(text) {
      masked_text = mask_phone_number(text, config)
      detected_pii.push(PhoneNumber)
    }
    
    // SSN检测
    if config.enable_ssn_detection and is_ssn(text) {
      masked_text = mask_ssn(text, config)
      detected_pii.push(SSN)
    }
    
    // 信用卡检测
    if config.enable_credit_card_detection and is_credit_card(text) {
      masked_text = mask_credit_card(text, config)
      detected_pii.push(CreditCard)
    }
    
    // IP地址检测
    if config.enable_ip_detection and is_ip_address(text) {
      masked_text = mask_ip_address(text, config)
      detected_pii.push(IPAddress)
    }
    
    // 地址检测
    if config.enable_address_detection and is_address(text) {
      masked_text = mask_address(text, config)
      detected_pii.push(Address)
    }
    
    (masked_text, detected_pii)
  }
  
  // 5. 测试PII检测和遮蔽
  let mut i = 0
  while i < pii_samples.length() {
    let sample = pii_samples[i]
    let (masked_text, detected_pii) = detect_and_mask_pii(sample.original, pii_config)
    
    // 验证检测结果
    if sample.should_detect {
      assert_eq(detected_pii.length() > 0, true)
      assert_eq(detected_pii.contains(sample.pii_type), true)
    }
    
    // 验证遮蔽结果
    if sample.should_detect {
      assert_eq(masked_text != sample.original, true)
      assert_eq(masked_text.length() <= sample.original.length(), true)
    }
    
    i = i + 1
  }
}

test "data_retention_and_expiry" {
  // 测试数据保留和过期策略
  
  // 1. 定义数据保留策略
  struct RetentionPolicy {
    data_type : String
    retention_days : Int
    auto_delete : Bool
    anonymize_after_expiry : Bool
    grace_period_days : Int
  }
  
  let retention_policies = [
    RetentionPolicy::{
      data_type: "user_activity",
      retention_days: 90,
      auto_delete: true,
      anonymize_after_expiry: false,
      grace_period_days: 7
    },
    RetentionPolicy::{
      data_type: "error_logs",
      retention_days: 30,
      auto_delete: true,
      anonymize_after_expiry: false,
      grace_period_days: 3
    },
    RetentionPolicy::{
      data_type: "performance_metrics",
      retention_days: 365,
      auto_delete: false,
      anonymize_after_expiry: true,
      grace_period_days: 30
    },
    RetentionPolicy::{
      data_type: "security_events",
      retention_days: 2555,  // 7年
      auto_delete: false,
      anonymize_after_expiry: true,
      grace_period_days: 90
    },
    RetentionPolicy::{
      data_type: "audit_trails",
      retention_days: 3650,  // 10年
      auto_delete: false,
      anonymize_after_expiry: false,
      grace_period_days: 365
    }
  ]
  
  // 2. 定义数据记录
  struct DataRecord {
    id : String
    data_type : String
    created_at : Int64  // Unix timestamp in nanoseconds
    data : String
    user_id : String?
    should_retain : Bool
  }
  
  let current_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let nanoseconds_per_day = 86400000000000L
  
  let data_records = [
    DataRecord::{
      id: "record1",
      data_type: "user_activity",
      created_at: current_time - (10 * nanoseconds_per_day),  // 10天前
      data: "User logged in",
      user_id: Some("user123"),
      should_retain: true
    },
    DataRecord::{
      id: "record2",
      data_type: "error_logs",
      created_at: current_time - (35 * nanoseconds_per_day),  // 35天前
      data: "Database connection failed",
      user_id: Some("user456"),
      should_retain: false
    },
    DataRecord::{
      id: "record3",
      data_type: "performance_metrics",
      created_at: current_time - (400 * nanoseconds_per_day),  // 400天前
      data: "Response time: 150ms",
      user_id: Some("user789"),
      should_retain: true  // 应该匿名化但不删除
    },
    DataRecord::{
      id: "record4",
      data_type: "security_events",
      created_at: current_time - (100 * nanoseconds_per_day),  // 100天前
      data: "Failed login attempt",
      user_id: Some("user101"),
      should_retain: true
    },
    DataRecord::{
      id: "record5",
      data_type: "audit_trails",
      created_at: current_time - (100 * nanoseconds_per_day),  // 100天前
      data: "Data export performed",
      user_id: Some("admin1"),
      should_retain: true
    }
  ]
  
  // 3. 实现数据保留检查函数
  fn should_retain_data(record : DataRecord, policy : RetentionPolicy, current_time : Int64) -> (Bool, Bool) {
    let age_days = (current_time - record.created_at) / 86400000000000L
    let retention_expired = age_days > policy.retention_days
    let in_grace_period = age_days <= (policy.retention_days + policy.grace_period_days)
    
    if not retention_expired {
      (true, false)  // 保留，不需要匿名化
    } else if in_grace_period {
      (true, false)  // 宽限期内保留
    } else if policy.anonymize_after_expiry {
      (true, true)   // 保留但需要匿名化
    } else if policy.auto_delete {
      (false, false)  // 删除
    } else {
      (true, false)   // 永久保留
    }
  }
  
  // 4. 实现数据匿名化函数
  fn anonymize_data(record : DataRecord) -> DataRecord {
    let anonymized_data = record.data
      .replace("user123", "user-***")
      .replace("user456", "user-***")
      .replace("user789", "user-***")
      .replace("user101", "user-***")
      .replace("admin1", "admin-***")
    
    DataRecord::{
      id: record.id,
      data_type: record.data_type,
      created_at: record.created_at,
      data: anonymized_data,
      user_id: None,  // 移除用户ID
      should_retain: record.should_retain
    }
  }
  
  // 5. 测试数据保留策略
  let mut i = 0
  while i < data_records.length() {
    let record = data_records[i]
    
    // 查找对应的保留策略
    let mut policy = retention_policies[0]  // 默认策略
    let mut j = 0
    while j < retention_policies.length() {
      if retention_policies[j].data_type == record.data_type {
        policy = retention_policies[j]
        break
      }
      j = j + 1
    }
    
    // 检查是否应该保留
    let (should_retain, should_anonymize) = should_retain_data(record, policy, current_time)
    
    // 验证保留策略
    if should_anonymize {
      let anonymized_record = anonymize_data(record)
      match anonymized_record.user_id {
        Some(_) => @test.fail("Test failed")
        None => assert_eq(true, true)
      }
      assert_eq(anonymized_record.data.contains("user-***"), true)
    }
    
    i = i + 1
  }
}

test "consent_management" {
  // 测试同意管理
  
  // 1. 定义同意类型
  enum ConsentType {
    Analytics
    Marketing
    Personalization
    ThirdPartySharing
    Cookies
    EmailCommunication
    SMSCommunication
    PushNotifications
    LocationTracking
    BiometricData
  }
  
  // 2. 定义同意记录
  struct ConsentRecord {
    user_id : String
    consent_type : ConsentType
    granted : Bool
    timestamp : Int64
    purpose : String
    legal_basis : String
    expiry_date : Int64?
  }
  
  // 3. 定义同意配置
  struct ConsentConfig {
    require_explicit_consent : Bool
    default_consent : Bool
    consent_expiry_days : Int
    allow_withdrawal : Bool
    withdrawal_effect : String  // "delete", "anonymize", "stop_collection"
  }
  
  let consent_config = ConsentConfig::{
    require_explicit_consent: true,
    default_consent: false,
    consent_expiry_days: 365,
    allow_withdrawal: true,
    withdrawal_effect: "anonymize"
  }
  
  // 4. 创建测试同意记录
  let consent_records = [
    ConsentRecord::{
      user_id: "user123",
      consent_type: Analytics,
      granted: true,
      timestamp: 1640995200000000000L,
      purpose: "Product improvement and performance monitoring",
      legal_basis: "Legitimate interest",
      expiry_date: Some(1640995200000000000L + (365 * 86400000000000L))
    },
    ConsentRecord::{
      user_id: "user123",
      consent_type: Marketing,
      granted: false,
      timestamp: 1640995200000000000L,
      purpose: "Promotional communications",
      legal_basis: "Consent",
      expiry_date: Some(1640995200000000000L + (365 * 86400000000000L))
    },
    ConsentRecord::{
      user_id: "user456",
      consent_type: Personalization,
      granted: true,
      timestamp: 1640995200000000000L,
      purpose: "Personalized user experience",
      legal_basis: "Consent",
      expiry_date: Some(1640995200000000000L + (180 * 86400000000000L))
    },
    ConsentRecord::{
      user_id: "user456",
      consent_type: ThirdPartySharing,
      granted: false,
      timestamp: 1640995200000000000L,
      purpose: "Data sharing with partners",
      legal_basis: "Consent",
      expiry_date: Some(1640995200000000000L + (365 * 86400000000000L))
    }
  ]
  
  // 5. 实现同意检查函数
  fn has_consent(user_id : String, consent_type : ConsentType, records : Array[ConsentRecord], current_time : Int64) -> Bool {
    let mut i = 0
    while i < records.length() {
      let record = records[i]
      if record.user_id == user_id and record.consent_type == consent_type {
        // 检查同意是否过期
        match record.expiry_date {
          Some(expiry) => {
            if current_time > expiry {
              return false  // 同意已过期
            }
          }
          None => {}
        }
        return record.granted
      }
      i = i + 1
    }
    false  // 未找到同意记录
  }
  
  // 6. 实现同意撤回函数
  fn withdraw_consent(user_id : String, consent_type : ConsentType, records : Array[ConsentRecord]) -> Array[ConsentRecord] {
    let updated_records = []
    let mut i = 0
    while i < records.length() {
      let record = records[i]
      if record.user_id == user_id and record.consent_type == consent_type {
        updated_records.push(ConsentRecord::{
          user_id: record.user_id,
          consent_type: record.consent_type,
          granted: false,  // 撤回同意
          timestamp: record.timestamp,
          purpose: record.purpose,
          legal_basis: record.legal_basis,
          expiry_date: record.expiry_date
        })
      } else {
        updated_records.push(record)
      }
      i = i + 1
    }
    updated_records
  }
  
  // 7. 测试同意管理
  let current_time = 1640995200000000000L
  
  // 测试同意检查
  let analytics_consent = has_consent("user123", Analytics, consent_records, current_time)
  let marketing_consent = has_consent("user123", Marketing, consent_records, current_time)
  let personalization_consent = has_consent("user456", Personalization, consent_records, current_time)
  let third_party_consent = has_consent("user456", ThirdPartySharing, consent_records, current_time)
  
  assert_eq(analytics_consent, true)
  assert_eq(marketing_consent, false)
  assert_eq(personalization_consent, true)
  assert_eq(third_party_consent, false)
  
  // 测试同意撤回
  let updated_records = withdraw_consent("user123", Analytics, consent_records)
  let withdrawn_consent = has_consent("user123", Analytics, updated_records, current_time)
  assert_eq(withdrawn_consent, false)
  
  // 测试过期同意
  let future_time = current_time + (400 * 86400000000000L)  // 400天后
  let expired_consent = has_consent("user456", Personalization, consent_records, future_time)
  assert_eq(expired_consent, false)
}

test "data_subject_rights" {
  // 测试数据主体权利（GDPR等）
  
  // 1. 定义数据主体权利类型
  enum DataSubjectRight {
    RightToAccess
    RightToRectification
    RightToErasure  // 被遗忘权
    RightToPortability
    RightToObject
    RightToRestrictProcessing
    RightToBeInformed
    RightNotToBeSubjectToAutomatedDecisionMaking
  }
  
  // 2. 定义数据主体请求
  struct DataSubjectRequest {
    request_id : String
    user_id : String
    right_type : DataSubjectRight
    request_data : String?
    timestamp : Int64
    status : String  // "pending", "approved", "rejected", "completed"
    processed_at : Int64?
  }
  
  // 3. 定义用户数据记录
  struct UserData {
    user_id : String
    personal_data : String
    behavioral_data : String
    transaction_data : String
    created_at : Int64
    updated_at : Int64
    processing_purposes : Array[String]
    legal_basis : String
  }
  
  // 4. 创建测试数据
  let user_data = UserData::{
    user_id: "user123",
    personal_data: "John Doe, john.doe@example.com, +1-555-0123",
    behavioral_data: "Page views: 150, Clicks: 300, Session duration: 45min",
    transaction_data: "Orders: 5, Total spent: $250.00, Last purchase: 2022-01-01",
    created_at: 1640995200000000000L,
    updated_at: 1640995200000000000L,
    processing_purposes: ["service_provision", "analytics", "marketing"],
    legal_basis: "consent"
  }
  
  // 5. 实现数据访问权利
  fn provide_data_access(user_data : UserData) -> String {
    "Personal Data: " + user_data.personal_data + "\n" +
    "Behavioral Data: " + user_data.behavioral_data + "\n" +
    "Transaction Data: " + user_data.transaction_data + "\n" +
    "Processing Purposes: " + join_strings(user_data.processing_purposes, ", ") + "\n" +
    "Legal Basis: " + user_data.legal_basis + "\n" +
    "Data Created: " + user_data.created_at.to_string() + "\n" +
    "Data Updated: " + user_data.updated_at.to_string()
  }
  
  // 6. 实现数据删除权利
  fn erase_user_data(user_data : UserData) -> UserData {
    UserData::{
      user_id: user_data.user_id,
      personal_data: "[ERASED]",
      behavioral_data: "[ERASED]",
      transaction_data: "[ERASED]",
      created_at: user_data.created_at,
      updated_at: 1640995200000000000L,  // 当前时间
      processing_purposes: [],
      legal_basis: "erased"
    }
  }
  
  // 7. 实现数据可携带性权利
  fn export_user_data(user_data : UserData) -> String {
    "{\n" +
    "  \"user_id\": \"" + user_data.user_id + "\",\n" +
    "  \"personal_data\": \"" + user_data.personal_data + "\",\n" +
    "  \"behavioral_data\": \"" + user_data.behavioral_data + "\",\n" +
    "  \"transaction_data\": \"" + user_data.transaction_data + "\",\n" +
    "  \"processing_purposes: " + array_to_json(user_data.processing_purposes) + ",\n" +
    "  \"legal_basis\": \"" + user_data.legal_basis + "\",\n" +
    "  \"created_at\": " + user_data.created_at.to_string() + ",\n" +
    "  \"updated_at\": " + user_data.updated_at.to_string() + "\n" +
    "}"
  }
  
  // 8. 测试数据主体权利
  let access_data = provide_data_access(user_data)
  assert_eq(access_data.contains("John Doe"), true)
  assert_eq(access_data.contains("john.doe@example.com"), true)
  assert_eq(access_data.contains("service_provision"), true)
  
  let erased_data = erase_user_data(user_data)
  assert_eq(erased_data.personal_data, "[ERASED]")
  assert_eq(erased_data.behavioral_data, "[ERASED]")
  assert_eq(erased_data.transaction_data, "[ERASED]")
  assert_eq(erased_data.processing_purposes.length(), 0)
  assert_eq(erased_data.legal_basis, "erased")
  
  let portable_data = export_user_data(user_data)
  assert_eq(portable_data.contains("\"user_id\": \"user123\""), true)
  assert_eq(portable_data.contains("\"personal_data\": \"John Doe"), true)
  assert_eq(portable_data.contains("\"legal_basis\": \"consent\""), true)
}

// 辅助函数
fn is_email(text : String) -> Bool {
  text.contains("@") and text.contains(".") and text.length() > 5
}

fn is_phone_number(text : String) -> Bool {
  let digits_only = text.replace("-", "").replace(" ", "").replace("(", "").replace(")", "")
  digits_only.length() >= 10 and is_digits(digits_only)
}

fn is_ssn(text : String) -> Bool {
  let pattern1 = text.length() == 11 and text.char_at(3) == '-' and text.char_at(6) == '-'
  let pattern2 = text.length() == 9 and is_digits(text)
  pattern1 or pattern2
}

fn is_credit_card(text : String) -> Bool {
  let digits_only = text.replace("-", "").replace(" ", "")
  digits_only.length() >= 13 and digits_only.length() <= 19 and is_digits(digits_only)
}

fn is_ip_address(text : String) -> Bool {
  // 简化的IPv4检测
  let parts = text".".split_to_string()
  if parts.length() == 4 {
    let mut i = 0
    while i < parts.length() {
      if not is_digits(parts[i]) or parts[i].to_int() > 255 {
        return false
      }
      i = i + 1
    }
    return true
  }
  false
}

fn is_address(text : String) -> Bool {
  // 简化的地址检测
  text.contains("St") or text.contains("Ave") or text.contains("Road") or 
  text.contains("街") or text.contains("路") or text.contains("号")
}

fn mask_email(email : String, config : PIIDetectionConfig) -> String {
  let parts = email"@".split_to_string()
  if parts.length() == 2 {
    let username = parts[0]
    let domain = parts[1]
    let masked_username = mask_string(username, 1, username.length() - 1)
    let domain_parts = domain".".split_to_string()
    if domain_parts.length() >= 2 {
      let masked_domain = mask_string(domain_parts[0], 1, domain_parts[0].length() - 1)
      let extension = domain_parts[domain_parts.length() - 1]
      return masked_username + "@" + masked_domain + "." + extension
    }
  }
  email
}

fn mask_phone_number(phone : String, config : PIIDetectionConfig) -> String {
  let digits = phone.replace("-", "").replace(" ", "").replace("(", "").replace(")", "")
  if digits.length() >= 10 {
    let visible_start = digits.length() - 4
    return mask_string(phone, 0, phone.length() - 4)
  }
  phone
}

fn mask_ssn(ssn : String, config : PIIDetectionConfig) -> String {
  "***-**-****"
}

fn mask_credit_card(card : String, config : PIIDetectionConfig) -> String {
  let digits = card.replace("-", "").replace(" ", "")
  if digits.length() >= 13 {
    let last4 = digits.substring(digits.length() - 4, 4)
    return "****-****-****-" + last4
  }
  card
}

fn mask_ip_address(ip : String, config : PIIDetectionConfig) -> String {
  let parts = ip".".split_to_string()
  if parts.length() == 4 {
    return parts[0] + "." + parts[1] + "." + parts[2] + ".***"
  }
  ip
}

fn mask_address(address : String, config : PIIDetectionConfig) -> String {
  let words = address" ".split_to_string()
  let masked_words = []
  let mut i = 0
  while i < words.length() {
    let word = words[i]
    if word.length() > 2 {
      masked_words.push(mask_string(word, 0, word.length() - 1))
    } else {
      masked_words.push(word)
    }
    i = i + 1
  }
  join_strings(masked_words, " ")
}

fn mask_string(input : String, start_visible : Int, end_visible : Int) -> String {
  if input.length() <= (start_visible + end_visible) {
    return input
  }
  
  let start = input.substring(0, start_visible)
  let end = input.substring(input.length() - end_visible, end_visible)
  let mask_length = input.length() - start_visible - end_visible
  let mask = "*".repeat(mask_length)
  
  start + mask + end
}

fn is_digits(text : String) -> Bool {
  let mut i = 0
  while i < text.length() {
    let char = text.char_at(i)
    if char < '0' or char > '9' {
      return false
    }
    i = i + 1
  }
  true
}

fn join_strings(strings : Array[String], separator : String) -> String {
  if strings.length() == 0 {
    return ""
  }
  
  let mut result = strings[0]
  let mut i = 1
  while i < strings.length() {
    result = result + separator + strings[i]
    i = i + 1
  }
  result
}

fn array_to_json(strings : Array[String]) -> String {
  let mut result = "["
  let mut i = 0
  while i < strings.length() {
    if i > 0 {
      result = result + ", "
    }
    result = result + "\"" + strings[i] + "\""
    i = i + 1
  }
  result = result + "]"
  result
}