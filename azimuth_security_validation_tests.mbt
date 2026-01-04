// Security Validation Test Suite for Azimuth
// 安全验证测试套件 - 测试各种安全验证和防护技术

// Test 1: 输入验证和清理
test "input_validation_and_sanitization" {
  // 字符串输入验证
  let validate_string = fn(input, rules) {
    let mut result = {
      valid: true,
      errors: [],
      sanitized: input
    }
    
    // 长度检查
    if rules.min_length && input.length() < rules.min_length {
      result.valid := false
      result.errors.push("Input too short")
    }
    
    if rules.max_length && input.length() > rules.max_length {
      result.valid := false
      result.errors.push("Input too long")
    }
    
    // 字符集检查
    if rules.allowed_chars {
      let mut invalid_chars = []
      for char in input {
        if !rules.allowed_chars.contains(char) {
          invalid_chars.push(char)
        }
      }
      
      if !invalid_chars.is_empty() {
        result.valid := false
        result.errors.push("Invalid characters: " + invalid_chars.join(""))
      }
    }
    
    // 危险内容检查
    if rules.dangerous_patterns {
      for pattern in rules.dangerous_patterns {
        if input.contains(pattern) {
          result.valid := false
          result.errors.push("Dangerous pattern detected: " + pattern)
        }
      }
    }
    
    // 清理输入
    if rules.sanitize_html {
      let mut sanitized = ""
      for char in input {
        match char {
          "<" => sanitized = sanitized + "&lt;"
          ">" => sanitized = sanitized + "&gt;"
          "&" => sanitized = sanitized + "&amp;"
          "\"" => sanitized = sanitized + "&quot;"
          "'" => sanitized = sanitized + "&#x27;"
          _ => sanitized = sanitized + char.to_string()
        }
      }
      result.sanitized := sanitized
    }
    
    result
  }
  
  let username_rules = {
    min_length: 3,
    max_length: 20,
    allowed_chars: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_",
    dangerous_patterns: ["<script>", "javascript:", "onload="],
    sanitize_html: true
  }
  
  let valid_input = "user123"
  let invalid_input = "us<script>alert('xss')</script>"
  let short_input = "us"
  let long_input = "this_is_a_very_long_username_that_exceeds_limit"
  
  let valid_result = validate_string(valid_input, username_rules)
  assert_true(valid_result.valid)
  assert_eq(valid_result.sanitized, "user123")
  
  let invalid_result = validate_string(invalid_input, username_rules)
  assert_false(invalid_result.valid)
  assert_eq(invalid_result.sanitized, "us&lt;script&gt;alert('xss')&lt;/script&gt;")
  
  let short_result = validate_string(short_input, username_rules)
  assert_false(short_result.valid)
  assert_true(short_result.errors.contains("Input too short"))
  
  let long_result = validate_string(long_input, username_rules)
  assert_false(long_result.valid)
  assert_true(long_result.errors.contains("Input too long"))
  
  // 数值输入验证
  let validate_number = fn(input, rules) {
    let mut result = {
      valid: true,
      errors: [],
      value: None
    }
    
    // 检查是否为数字
    let num_value = input.to_float()
    match num_value {
      Some(num) => {
        // 范围检查
        if rules.min_value && num < rules.min_value {
          result.valid := false
          result.errors.push("Value too small")
        }
        
        if rules.max_value && num > rules.max_value {
          result.valid := false
          result.errors.push("Value too large")
        }
        
        result.value := Some(num)
      }
      None => {
        result.valid := false
        result.errors.push("Not a valid number")
      }
    }
    
    result
  }
  
  let age_rules = {
    min_value: 0.0,
    max_value: 150.0
  }
  
  let valid_age = validate_number("25", age_rules)
  assert_true(valid_age.valid)
  assert_eq(valid_age.value, Some(25.0))
  
  let invalid_age = validate_number("abc", age_rules)
  assert_false(invalid_age.valid)
  assert_true(invalid_age.errors.contains("Not a valid number"))
  
  let out_of_range_age = validate_number("200", age_rules)
  assert_false(out_of_range_age.valid)
  assert_true(out_of_range_age.errors.contains("Value too large"))
}

// Test 2: 密码安全性
test "password_security" {
  // 密码强度检查
  let check_password_strength = fn(password) {
    let mut score = 0
    let mut feedback = []
    
    // 长度检查
    if password.length() >= 8 {
      score = score + 1
    } else {
      feedback.push("Password should be at least 8 characters")
    }
    
    if password.length() >= 12 {
      score = score + 1
    }
    
    // 字符类型检查
    let has_lowercase = password.any(fn(c) { c >= 'a' && c <= 'z' })
    let has_uppercase = password.any(fn(c) { c >= 'A' && c <= 'Z' })
    let has_digits = password.any(fn(c) { c >= '0' && c <= '9' })
    let has_special = password.any(fn(c) { !"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".contains(c) })
    
    if has_lowercase {
      score = score + 1
    } else {
      feedback.push("Password should contain lowercase letters")
    }
    
    if has_uppercase {
      score = score + 1
    } else {
      feedback.push("Password should contain uppercase letters")
    }
    
    if has_digits {
      score = score + 1
    } else {
      feedback.push("Password should contain digits")
    }
    
    if has_special {
      score = score + 1
    } else {
      feedback.push("Password should contain special characters")
    }
    
    // 常见密码检查
    let common_passwords = ["password", "123456", "qwerty", "admin", "letmein"]
    if common_passwords.any(fn(common) { password.to_lowercase() == common }) {
      score = score - 2
      feedback.push("Password is too common")
    }
    
    // 重复字符检查
    let mut has_repeats = false
    for i in 0..(password.length() - 2) {
      if password[i] == password[i + 1] && password[i + 1] == password[i + 2] {
        has_repeats = true
        break
      }
    }
    
    if has_repeats {
      score = score - 1
      feedback.push("Password contains repeating characters")
    }
    
    let strength = if score >= 6 {
      "strong"
    } else if score >= 4 {
      "medium"
    } else {
      "weak"
    }
    
    {
      score: score,
      strength: strength,
      feedback: feedback
    }
  }
  
  let strong_password = "MyStr0ng!P@ssw0rd"
  let weak_password = "password"
  let medium_password = "Password123"
  
  let strong_result = check_password_strength(strong_password)
  assert_eq(strong_result.strength, "strong")
  assert_true(strong_result.feedback.is_empty())
  
  let weak_result = check_password_strength(weak_password)
  assert_eq(weak_result.strength, "weak")
  assert_false(weak_result.feedback.is_empty())
  
  let medium_result = check_password_strength(medium_password)
  assert_eq(medium_result.strength, "medium")
  
  // 密码哈希和验证
  let hash_password = fn(password, salt) {
    // 简化的哈希函数（实际应用中应使用安全的哈希算法）
    let mut hash = 0
    for c in password + salt {
      hash = (hash * 31 + c.to_int()) % 1000000
    }
    hash.to_string()
  }
  
  let verify_password = fn(password, salt, hash) {
    let computed_hash = hash_password(password, salt)
    computed_hash == hash
  }
  
  let password = "user_password"
  let salt = "random_salt_123"
  let hashed = hash_password(password, salt)
  
  assert_true(verify_password(password, salt, hashed))
  assert_false(verify_password("wrong_password", salt, hashed))
  
  // 密码策略
  let password_policy = {
    min_length: 8,
    max_length: 128,
    require_uppercase: true,
    require_lowercase: true,
    require_digits: true,
    require_special: true,
    forbidden_patterns: ["password", "123456", "qwerty"],
    max_age_days: 90
  }
  
  let validate_password_policy = fn(password, policy) {
    let mut valid = true
    let mut errors = []
    
    if password.length() < policy.min_length {
      valid := false
      errors.push("Password too short")
    }
    
    if password.length() > policy.max_length {
      valid := false
      errors.push("Password too long")
    }
    
    if policy.require_uppercase && !password.any(fn(c) { c >= 'A' && c <= 'Z' }) {
      valid := false
      errors.push("Password must contain uppercase letters")
    }
    
    if policy.require_lowercase && !password.any(fn(c) { c >= 'a' && c <= 'z' }) {
      valid := false
      errors.push("Password must contain lowercase letters")
    }
    
    if policy.require_digits && !password.any(fn(c) { c >= '0' && c <= '9' }) {
      valid := false
      errors.push("Password must contain digits")
    }
    
    if policy.require_special && !password.any(fn(c) { !"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".contains(c) }) {
      valid := false
      errors.push("Password must contain special characters")
    }
    
    let password_lower = password.to_lowercase()
    for pattern in policy.forbidden_patterns {
      if password_lower.contains(pattern) {
        valid := false
        errors.push("Password contains forbidden pattern: " + pattern)
      }
    }
    
    { valid, errors }
  }
  
  let policy_result = validate_password_policy("MyP@ssw0rd", password_policy)
  assert_true(policy_result.valid)
  assert_true(policy_result.errors.is_empty())
  
  let policy_invalid = validate_password_policy("weak", password_policy)
  assert_false(policy_invalid.valid)
  assert_false(policy_invalid.errors.is_empty())
}

// Test 3: 访问控制和权限
test "access_control_and_permissions" {
  // 权限定义
  type Permission {
    Read
    Write
    Delete
    Admin
  }
  
  type Role {
    User
    Moderator
    Admin
  }
  
  // 角色权限映射
  let role_permissions = {
    User: [Permission.Read],
    Moderator: [Permission.Read, Permission.Write],
    Admin: [Permission.Read, Permission.Write, Permission.Delete, Permission.Admin]
  }
  
  // 检查权限
  let has_permission = fn(role, permission) {
    let permissions = role_permissions.get(role).unwrap_or([])
    permissions.contains(permission)
  }
  
  assert_true(has_permission(Role.User, Permission.Read))
  assert_false(has_permission(Role.User, Permission.Write))
  assert_false(has_permission(Role.User, Permission.Delete))
  
  assert_true(has_permission(Role.Moderator, Permission.Read))
  assert_true(has_permission(Role.Moderator, Permission.Write))
  assert_false(has_permission(Role.Moderator, Permission.Delete))
  
  assert_true(has_permission(Role.Admin, Permission.Read))
  assert_true(has_permission(Role.Admin, Permission.Write))
  assert_true(has_permission(Role.Admin, Permission.Delete))
  assert_true(has_permission(Role.Admin, Permission.Admin))
  
  // 资源访问控制
  type Resource {
    id: String
    owner: String
    permissions: Map[String, Array[Permission]]
  }
  
  let check_resource_access = fn(resource, user_id, permission) {
    // 所有者拥有所有权限
    if resource.owner == user_id {
      return true
    }
    
    // 检查用户特定权限
    match resource.permissions.get(user_id) {
      Some(user_permissions) => {
        user_permissions.contains(permission)
      }
      None => false
    }
  }
  
  let resource = {
    id: "doc123",
    owner: "user1",
    permissions: {
      "user2": [Permission.Read],
      "user3": [Permission.Read, Permission.Write]
    }
  }
  
  assert_true(check_resource_access(resource, "user1", Permission.Delete))  // 所有者
  assert_true(check_resource_access(resource, "user2", Permission.Read))   // 显式权限
  assert_false(check_resource_access(resource, "user2", Permission.Write)) // 无权限
  assert_true(check_resource_access(resource, "user3", Permission.Write))  // 有权限
  assert_false(check_resource_access(resource, "user4", Permission.Read))  // 无权限
  
  // 基于属性的访问控制 (ABAC)
  type Attribute {
    key: String
    value: String
  }
  
  let evaluate_access_policy = fn(subject_attributes, resource_attributes, action, environment_attributes) {
    // 简化的策略评估
    let subject_role = subject_attributes.get("role").unwrap_or("")
    let resource_sensitivity = resource_attributes.get("sensitivity").unwrap_or("")
    let action_type = action
    let time_of_day = environment_attributes.get("time_of_day").unwrap_or("")
    
    // 规则1: 只有管理员可以访问高敏感度资源
    if resource_sensitivity == "high" && subject_role != "admin" {
      return false
    }
    
    // 规则2: 普通用户只能在工作时间访问
    if subject_role == "user" && (time_of_day < "09:00" || time_of_day > "17:00") {
      return false
    }
    
    // 规则3: 删除操作需要管理员或所有者权限
    if action_type == "delete" && subject_role != "admin" {
      return false
    }
    
    true
  }
  
  let user_attributes = {
    "role": "user",
    "department": "engineering"
  }
  
  let admin_attributes = {
    "role": "admin",
    "department": "IT"
  }
  
  let public_resource = {
    "sensitivity": "public",
    "type": "document"
  }
  
  let sensitive_resource = {
    "sensitivity": "high",
    "type": "document"
  }
  
  let work_hours = {
    "time_of_day": "10:00",
    "day_of_week": "Monday"
  }
  
  let after_hours = {
    "time_of_day": "20:00",
    "day_of_week": "Monday"
  }
  
  assert_true(evaluate_access_policy(user_attributes, public_resource, "read", work_hours))
  assert_false(evaluate_access_policy(user_attributes, public_resource, "read", after_hours))
  assert_false(evaluate_access_policy(user_attributes, sensitive_resource, "read", work_hours))
  assert_false(evaluate_access_policy(user_attributes, public_resource, "delete", work_hours))
  
  assert_true(evaluate_access_policy(admin_attributes, sensitive_resource, "read", work_hours))
  assert_true(evaluate_access_policy(admin_attributes, sensitive_resource, "delete", work_hours))
}

// Test 4: 加密和解密
test "encryption_and_decryption" {
  // 简单的凯撒密码（仅用于演示，实际应用中使用强加密算法）
  let caesar_encrypt = fn(text, shift) {
    let mut result = ""
    
    for char in text {
      if char >= 'a' && char <= 'z' {
        let shifted = ((char.to_int() - 'a'.to_int() + shift) % 26) + 'a'.to_int()
        result = result + shifted.to_char().to_string()
      } else if char >= 'A' && char <= 'Z' {
        let shifted = ((char.to_int() - 'A'.to_int() + shift) % 26) + 'A'.to_int()
        result = result + shifted.to_char().to_string()
      } else {
        result = result + char.to_string()
      }
    }
    
    result
  }
  
  let caesar_decrypt = fn(text, shift) {
    caesar_encrypt(text, 26 - shift)
  }
  
  let plaintext = "Hello, World!"
  let shift = 3
  let encrypted = caesar_encrypt(plaintext, shift)
  let decrypted = caesar_decrypt(encrypted, shift)
  
  assert_eq(encrypted, "Khoor, Zruog!")
  assert_eq(decrypted, plaintext)
  
  // XOR加密（同样仅用于演示）
  let xor_encrypt = fn(data, key) {
    let mut result = []
    let key_len = key.length()
    
    for i in 0..data.length() {
      let key_byte = key[i % key_len].to_int()
      let data_byte = data[i].to_int()
      result.push((data_byte ^ key_byte).to_char())
    }
    
    result.join("")
  }
  
  let xor_decrypt = xor_encrypt  // XOR加密和解密是相同的操作
  
  let secret_data = "Secret message"
  let secret_key = "key123"
  let xor_encrypted = xor_encrypt(secret_data, secret_key)
  let xor_decrypted = xor_decrypt(xor_encrypted, secret_key)
  
  assert_eq(xor_decrypted, secret_data)
  
  // 密钥派生函数
  let derive_key = fn(password, salt, iterations) {
    let mut hash = password + salt
    for i in 0..iterations {
      // 简化的哈希迭代（实际应用中使用PBKDF2等标准算法）
      let mut new_hash = ""
      for c in hash {
        new_hash = new_hash + ((c.to_int() * 7 + 13) % 256).to_char().to_string()
      }
      hash = new_hash
    }
    hash
  }
  
  let password = "user_password"
  let salt = "random_salt"
  let derived_key = derive_key(password, salt, 1000)
  
  // 相同密码和盐应生成相同的密钥
  let derived_key2 = derive_key(password, salt, 1000)
  assert_eq(derived_key, derived_key2)
  
  // 不同密码应生成不同密钥
  let derived_key3 = derive_key("different_password", salt, 1000)
  assert_ne(derived_key, derived_key3)
  
  // 不同盐应生成不同密钥
  let derived_key4 = derive_key(password, "different_salt", 1000)
  assert_ne(derived_key, derived_key4)
  
  // 数字签名
  let create_signature = fn(message, private_key) {
    // 简化的签名算法（实际应用中使用RSA或ECDSA）
    let mut hash = 0
    for c in message {
      hash = (hash * 31 + c.to_int()) % 1000000
    }
    
    // 使用私钥"加密"哈希
    let mut signature = ""
    for c in private_key {
      signature = signature + ((c.to_int() + hash) % 256).to_char().to_string()
    }
    
    signature
  }
  
  let verify_signature = fn(message, signature, public_key) {
    // 简化的验证算法
    let mut hash = 0
    for c in message {
      hash = (hash * 31 + c.to_int()) % 1000000
    }
    
    // 使用公钥验证签名
    let mut expected_signature = ""
    for c in public_key {
      expected_signature = expected_signature + ((c.to_int() + hash) % 256).to_char().to_string()
    }
    
    signature == expected_signature
  }
  
  let message = "Important message"
  let private_key = "private_key_123"
  let public_key = "public_key_456"  // 在实际应用中，公钥和私钥有数学关系
  
  let signature = create_signature(message, private_key)
  assert_true(verify_signature(message, signature, public_key))
  
  // 篡改消息应导致验证失败
  assert_false(verify_signature("Tampered message", signature, public_key))
}

// Test 5: 会话管理
test "session_management" {
  // 会话存储
  let create_session_store = fn() {
    let sessions = ref {}
    let session_counter = ref 0
    
    fn(user_id, data) {
      // 创建会话
      session_counter := !session_counter + 1
      let session_id = "session_" + (!session_counter).to_string()
      let timestamp = current_time()  // 模拟当前时间
      
      let session = {
        id: session_id,
        user_id: user_id,
        data: data,
        created_at: timestamp,
        last_accessed: timestamp,
        expires_at: timestamp + 3600  // 1小时后过期
      }
      
      sessions.set(session_id, session)
      session_id
    },
    
    fn(session_id) {
      // 获取会话
      match (!sessions).get(session_id) {
        Some(session) => {
          // 检查是否过期
          if current_time() > session.expires_at {
            sessions.delete(session_id)
            None
          } else {
            // 更新最后访问时间
            let updated_session = { session with last_accessed: current_time() }
            sessions.set(session_id, updated_session)
            Some(updated_session)
          }
        }
        None => None
      }
    },
    
    fn(session_id) {
      // 删除会话
      sessions.delete(session_id)
    },
    
    fn(user_id) {
      // 删除用户的所有会话
      let mut to_delete = []
      for (id, session) in !sessions {
        if session.user_id == user_id {
          to_delete.push(id)
        }
      }
      
      for id in to_delete {
        sessions.delete(id)
      }
    },
    
    fn() {
      // 清理过期会话
      let mut to_delete = []
      let current = current_time()
      
      for (id, session) in !sessions {
        if current > session.expires_at {
          to_delete.push(id)
        }
      }
      
      for id in to_delete {
        sessions.delete(id)
      }
      
      to_delete.length()
    },
    
    fn() {
      // 获取活跃会话数
      (!sessions).size()
    }
  }
  
  let (create_session, get_session, delete_session, delete_user_sessions, cleanup_expired, active_sessions) = create_session_store()
  
  // 创建会话
  let session1 = create_session("user1", { name: "User One", role: "user" })
  let session2 = create_session("user2", { name: "User Two", role: "admin" })
  
  assert_eq(active_sessions(), 2)
  
  // 获取会话
  match get_session(session1) {
    Some(session) => {
      assert_eq(session.user_id, "user1")
      assert_eq(session.data.name, "User One")
    }
    None => assert_true(false)
  }
  
  // 删除会话
  delete_session(session1)
  assert_eq(get_session(session1), None)
  assert_eq(active_sessions(), 1)
  
  // 删除用户的所有会话
  let session3 = create_session("user1", { name: "User One", role: "user" })
  delete_user_sessions("user1")
  assert_eq(get_session(session3), None)
  assert_eq(active_sessions(), 1)
  
  // 会话令牌生成
  let generate_token = fn(length) {
    let charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let mut token = ""
    
    for i in 0..length {
      let index = (current_time() + i) % charset.length()
      token = token + charset[index].to_string()
    }
    
    token
  }
  
  let token = generate_token(32)
  assert_eq(token.length(), 32)
  
  // 令牌验证
  let validate_token = fn(token, expected_length) {
    if token.length() != expected_length {
      return false
    }
    
    // 检查是否只包含有效字符
    let charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    for char in token {
      if !charset.contains(char) {
        return false
      }
    }
    
    true
  }
  
  assert_true(validate_token(token, 32))
  assert_false(validate_token("invalid@token", 32))
  assert_false(validate_token("short", 32))
}

// Test 6: CSRF防护
test "csrf_protection" {
  // CSRF令牌生成
  let generate_csrf_token = fn(session_id, secret) {
    // 简化的令牌生成（实际应用中使用更强的算法）
    let combined = session_id + secret + current_time().to_string()
    let mut hash = 0
    for c in combined {
      hash = (hash * 31 + c.to_int()) % 1000000
    }
    session_id + "_" + hash.to_string()
  }
  
  // CSRF令牌验证
  let validate_csrf_token = fn(token, session_id, secret) {
    match token.split("_") {
      [token_session_id, token_hash] => {
        if token_session_id != session_id {
          return false
        }
        
        // 重新生成令牌进行比较
        let expected_token = generate_csrf_token(session_id, secret)
        token == expected_token
      }
      _ => false
    }
  }
  
  let session_id = "session123"
  let secret = "csrf_secret_key"
  
  let token = generate_csrf_token(session_id, secret)
  assert_true(validate_csrf_token(token, session_id, secret))
  
  // 不同会话应验证失败
  assert_false(validate_csrf_token(token, "different_session", secret))
  
  // 不同密钥应验证失败
  assert_false(validate_csrf_token(token, session_id, "different_secret"))
  
  // 双重提交Cookie模式
  let double_submit_cookie = fn(session_id, secret) {
    let cookie_token = generate_csrf_token(session_id, secret)
    let form_token = generate_csrf_token(session_id, secret)
    
    // 在实际应用中，cookie_token通过Cookie发送，form_token通过表单字段发送
    { cookie_token, form_token }
  }
  
  let validate_double_submit = fn(cookie_token, form_token) {
    // 简单比较两个令牌
    cookie_token == form_token
  }
  
  let { cookie_token, form_token } = double_submit_cookie(session_id, secret)
  assert_true(validate_double_submit(cookie_token, form_token))
  
  // 篡改令牌应验证失败
  assert_false(validate_double_submit(cookie_token, "tampered_token"))
  
  // SameSite Cookie属性模拟
  let same_site_validation = fn(request_origin, request_target, same_site_policy) {
    match same_site_policy {
      "Strict" => request_origin == request_target
      "Lax" => {
        // 简化：只允许安全方法和顶级导航
        request_origin == request_target || is_safe_method(request_origin) || is_top_level_navigation(request_target)
      }
      "None" => true  // 需要Secure属性
      _ => true
    }
  }
  
  let is_safe_method = fn(origin) {
    // 简化：假设GET、HEAD、OPTIONS是安全方法
    origin.contains("GET") || origin.contains("HEAD") || origin.contains("OPTIONS")
  }
  
  let is_top_level_navigation = fn(target) {
    // 简化：假设某些URL是顶级导航
    target.contains("/") && !target.contains("api/")
  }
  
  assert_true(same_site_validation("https://example.com", "https://example.com/api/data", "Strict"))
  assert_false(same_site_validation("https://evil.com", "https://example.com/api/data", "Strict"))
  
  assert_true(same_site_validation("https://example.com", "https://example.com/page", "Lax"))
  assert_true(same_site_validation("https://evil.com", "https://example.com/", "Lax"))  // 顶级导航
  assert_false(same_site_validation("https://evil.com", "https://example.com/api/data", "Lax"))
  
  assert_true(same_site_validation("https://evil.com", "https://example.com/api/data", "None"))
}

// Test 7: 内容安全策略
test "content_security_policy" {
  // CSP头部生成
  let generate_csp_header = fn(policies) {
    let mut directives = []
    
    if policies.default_src {
      directives.push("default-src " + policies.default_src.join(" "))
    }
    
    if policies.script_src {
      directives.push("script-src " + policies.script_src.join(" "))
    }
    
    if policies.style_src {
      directives.push("style-src " + policies.style_src.join(" "))
    }
    
    if policies.img_src {
      directives.push("img-src " + policies.img_src.join(" "))
    }
    
    if policies.connect_src {
      directives.push("connect-src " + policies.connect_src.join(" "))
    }
    
    if policies.font_src {
      directives.push("font-src " + policies.font_src.join(" "))
    }
    
    if policies.object_src {
      directives.push("object-src " + policies.object_src.join(" "))
    }
    
    if policies.media_src {
      directives.push("media-src " + policies.media_src.join(" "))
    }
    
    if policies.frame_src {
      directives.push("frame-src " + policies.frame_src.join(" "))
    }
    
    if policies.child_src {
      directives.push("child-src " + policies.child_src.join(" "))
    }
    
    if policies.worker_src {
      directives.push("worker-src " + policies.worker_src.join(" "))
    }
    
    if policies.manifest_src {
      directives.push("manifest-src " + policies.manifest_src.join(" "))
    }
    
    if policies.frame_ancestors {
      directives.push("frame-ancestors " + policies.frame_ancestors.join(" "))
    }
    
    if policies.base_uri {
      directives.push("base-uri " + policies.base_uri.join(" "))
    }
    
    if policies.form_action {
      directives.push("form-action " + policies.form_action.join(" "))
    }
    
    if policies.report_to {
      directives.push("report-to " + policies.report_to)
    }
    
    directives.join("; ")
  }
  
  let strict_csp = {
    default_src: ["'self'"],
    script_src: ["'self'", "'unsafe-inline'"],
    style_src: ["'self'", "'unsafe-inline'"],
    img_src: ["'self'", "data:", "https:"],
    connect_src: ["'self'"],
    font_src: ["'self'", "https:"],
    object_src: ["'none'"],
    media_src: ["'self'"],
    frame_src: ["'none'"],
    child_src: ["'none'"],
    worker_src: ["'self'"],
    manifest_src: ["'self'"],
    frame_ancestors: ["'none'"],
    base_uri: ["'self'"],
    form_action: ["'self'"],
    report_to: "csp-endpoint"
  }
  
  let csp_header = generate_csp_header(strict_csp)
  assert_true(csp_header.contains("default-src 'self'"))
  assert_true(csp_header.contains("script-src 'self' 'unsafe-inline'"))
  assert_true(csp_header.contains("object-src 'none'"))
  assert_true(csp_header.contains("frame-ancestors 'none'"))
  
  // CSP违规检测
  let detect_csp_violation = fn(request, csp_policies) {
    let url = request.url
    let resource_type = request.type
    let origin = request.origin
    
    // 检查资源类型是否匹配相应的CSP指令
    let allowed_sources = match resource_type {
      "script" => csp_policies.script_src
      "style" => csp_policies.style_src
      "img" => csp_policies.img_src
      "connect" => csp_policies.connect_src
      "font" => csp_policies.font_src
      "object" => csp_policies.object_src
      "media" => csp_policies.media_src
      "frame" => csp_policies.frame_src
      "child" => csp_policies.child_src
      "worker" => csp_policies.worker_src
      "manifest" => csp_policies.manifest_src
      _ => csp_policies.default_src
    }
    
    match allowed_sources {
      Some(sources) => {
        // 检查来源是否在允许列表中
        if sources.contains("'self'") && origin == "https://example.com" {
          false  // 不违规
        } else if sources.contains("'none'") {
          true   // 违规
        } else if sources.any(fn(source) { url.contains(source) }) {
          false  // 不违规
        } else {
          true   // 违规
        }
      }
      None => false  // 没有特定策略，不违规
    }
  }
  
  let legitimate_request = {
    url: "https://example.com/script.js",
    type: "script",
    origin: "https://example.com"
  }
  
  let malicious_request = {
    url: "https://evil.com/malicious.js",
    type: "script",
    origin: "https://evil.com"
  }
  
  assert_false(detect_csp_violation(legitimate_request, strict_csp))
  assert_true(detect_csp_violation(malicious_request, strict_csp))
}

// Test 8: 安全头部
test "security_headers" {
  // 生成安全头部
  let generate_security_headers = fn(options) {
    let mut headers = {}
    
    // X-Frame-Options
    if options.x_frame_options {
      headers.set("X-Frame-Options", options.x_frame_options)
    }
    
    // X-Content-Type-Options
    if options.x_content_type_options {
      headers.set("X-Content-Type-Options", options.x_content_type_options)
    }
    
    // X-XSS-Protection
    if options.x_xss_protection {
      headers.set("X-XSS-Protection", options.x_xss_protection)
    }
    
    // Strict-Transport-Security
    if options.strict_transport_security {
      headers.set("Strict-Transport-Security", options.strict_transport_security)
    }
    
    // Referrer-Policy
    if options.referrer_policy {
      headers.set("Referrer-Policy", options.referrer_policy)
    }
    
    // Feature-Policy
    if options.feature_policy {
      headers.set("Feature-Policy", options.feature_policy)
    }
    
    // Permissions-Policy
    if options.permissions_policy {
      headers.set("Permissions-Policy", options.permissions_policy)
    }
    
    headers
  }
  
  let security_options = {
    x_frame_options: "DENY",
    x_content_type_options: "nosniff",
    x_xss_protection: "1; mode=block",
    strict_transport_security: "max-age=31536000; includeSubDomains",
    referrer_policy: "strict-origin-when-cross-origin",
    feature_policy: "geolocation 'none'; microphone 'none'; camera 'none'",
    permissions_policy: "geolocation=(), microphone=(), camera=()"
  }
  
  let headers = generate_security_headers(security_options)
  
  assert_eq(headers.get("X-Frame-Options").unwrap_or(""), "DENY")
  assert_eq(headers.get("X-Content-Type-Options").unwrap_or(""), "nosniff")
  assert_eq(headers.get("X-XSS-Protection").unwrap_or(""), "1; mode=block")
  assert_eq(headers.get("Strict-Transport-Security").unwrap_or(""), "max-age=31536000; includeSubDomains")
  
  // 验证HSTS头部
  let validate_hsts_header = fn(header) {
    // 检查是否包含max-age
    if !header.contains("max-age=") {
      return false
    }
    
    // 检查max-age值是否为正数
    let max_age_match = header.match("max-age=([0-9]+)")
    match max_age_match {
      Some(age_str) => {
        let age = age_str.to_int().unwrap_or(0)
        age > 0
      }
      None => false
    }
  }
  
  assert_true(validate_hsts_header("max-age=31536000; includeSubDomains"))
  assert_false(validate_hsts_header("max-age=0"))  // 过期
  assert_false(validate_hsts_header("includeSubDomains"))  // 缺少max-age
  
  // 验证CSP头部
  let validate_csp_header = fn(header) {
    // 检查是否包含default-src或script-src
    header.contains("default-src") || header.contains("script-src")
  }
  
  assert_true(validate_csp_header("default-src 'self'; script-src 'self' 'unsafe-inline'"))
  assert_false(validate_csp_header("random-header: value"))
}

// Test 9: 日志和监控
test "logging_and_monitoring" {
  // 安全日志记录
  let create_security_logger = fn() {
    let logs = ref []
    
    fn(event_type, details) {
      let timestamp = current_time()
      let log_entry = {
        timestamp: timestamp,
        event_type: event_type,
        details: details
      }
      
      logs.push(log_entry)
      
      // 在实际应用中，这里应该发送到日志系统
    },
    
    fn(event_type, start_time, end_time) {
      // 查询日志
      let mut filtered_logs = []
      for log in !logs {
        if log.event_type == event_type && 
           log.timestamp >= start_time && 
           log.timestamp <= end_time {
          filtered_logs.push(log)
        }
      }
      filtered_logs
    },
    
    fn() {
      // 获取所有日志
      !logs
    },
    
    fn() {
      // 清理旧日志
      let cutoff_time = current_time() - 86400  // 24小时前
      let mut new_logs = []
      for log in !logs {
        if log.timestamp > cutoff_time {
          new_logs.push(log)
        }
      }
      logs := new_logs
    }
  }
  
  let (log_event, query_logs, get_all_logs, cleanup_old_logs) = create_security_logger()
  
  // 记录安全事件
  log_event("login_attempt", { user: "user1", ip: "192.168.1.1", success: true })
  log_event("login_attempt", { user: "user2", ip: "192.168.1.2", success: false })
  log_event("permission_denied", { user: "user3", resource: "/admin", action: "delete" })
  log_event("password_change", { user: "user1", success: true })
  
  // 查询日志
  let current_time = 1000000  // 模拟当前时间
  let start_time = current_time - 3600  // 1小时前
  let end_time = current_time + 3600    // 1小时后
  
  let login_attempts = query_logs("login_attempt", start_time, end_time)
  assert_eq(login_attempts.length(), 2)
  
  let permission_denied = query_logs("permission_denied", start_time, end_time)
  assert_eq(permission_denied.length(), 1)
  
  // 异常检测
  let detect_anomalies = fn(logs, threshold) {
    let mut anomalies = []
    let mut ip_counts = {}
    
    // 统计IP访问次数
    for log in logs {
      if log.event_type == "login_attempt" {
        let ip = log.details.get("ip").unwrap_or("")
        let current_count = ip_counts.get(ip).unwrap_or(0)
        ip_counts.set(ip, current_count + 1)
      }
    }
    
    // 检测异常IP
    for (ip, count) in ip_counts {
      if count > threshold {
        anomalies.push({
          type: "high_frequency_login",
          ip: ip,
          count: count
        })
      }
    }
    
    anomalies
  }
  
  // 添加更多登录尝试
  for i in 0..15 {
    log_event("login_attempt", { user: "attacker", ip: "10.0.0.1", success: false })
  }
  
  let all_logs = get_all_logs()
  let anomalies = detect_anomalies(all_logs, 10)
  
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].ip, "10.0.0.1")
  assert_eq(anomalies[0].count, 15)
}

// Test 10: 安全扫描和漏洞检测
test "security_scanning_and_vulnerability_detection" {
  // SQL注入检测
  let detect_sql_injection = fn(input) {
    let sql_patterns = [
      "'",
      "\"",
      "--",
      "/*",
      "*/",
      "xp_",
      "select",
      "insert",
      "update",
      "delete",
      "drop",
      "union",
      "exec",
      "script"
    ]
    
    let input_lower = input.to_lowercase()
    for pattern in sql_patterns {
      if input_lower.contains(pattern) {
        return true
      }
    }
    false
  }
  
  assert_true(detect_sql_injection("'; DROP TABLE users; --"))
  assert_true(detect_sql_injection("SELECT * FROM users WHERE id = 1"))
  assert_false(detect_sql_injection("normal user input"))
  
  // XSS检测
  let detect_xss = fn(input) {
    let xss_patterns = [
      "<script",
      "</script>",
      "javascript:",
      "onload=",
      "onerror=",
      "onclick=",
      "onmouseover=",
      "alert(",
      "document.cookie",
      "window.location",
      "eval("
    ]
    
    let input_lower = input.to_lowercase()
    for pattern in xss_patterns {
      if input_lower.contains(pattern) {
        return true
      }
    }
    false
  }
  
  assert_true(detect_xss("<script>alert('XSS')</script>"))
  assert_true(detect_xss("<img onload=\"alert('XSS')\" src=\"x\">"))
  assert_true(detect_xss("javascript:alert('XSS')"))
  assert_false(detect_xss("normal text content"))
  
  // 路径遍历检测
  let detect_path_traversal = fn(input) {
    let traversal_patterns = [
      "../",
      "..\\",
      "%2e%2e%2f",
      "%2e%2e\\",
      "..%2f",
      "..%5c",
      "%2e%2e/",
      "%2e%2e\\"
    ]
    
    for pattern in traversal_patterns {
      if input.contains(pattern) {
        return true
      }
    }
    false
  }
  
  assert_true(detect_path_traversal("../../../etc/passwd"))
  assert_true(detect_path_traversal("..%2f..%2f..%2fetc%2fpasswd"))
  assert_false(detect_path_traversal("normal/path/to/file"))
  
  // 命令注入检测
  let detect_command_injection = fn(input) {
    let command_patterns = [
      "&",
      "|",
      ";",
      "`",
      "$(",
      "${",
      "&&",
      "||",
      ">>",
      ">",
      "<",
      "'",
      "\"",
      "\\",
      "\n",
      "\r",
      "\t",
      " ",
      "!",
      "*",
      "?",
      "[",
      "]",
      "{",
      "}",
      "(", ")",
      "echo",
      "cat",
      "ls",
      "dir",
      "type",
      "whoami",
      "id",
      "pwd",
      "ps",
      "netstat",
      "ipconfig",
      "ifconfig",
      "ping",
      "curl",
      "wget",
      "nc",
      "telnet",
      "ssh"
    ]
    
    let input_lower = input.to_lowercase()
    for pattern in command_patterns {
      if input_lower.contains(pattern) {
        return true
      }
    }
    false
  }
  
  assert_true(detect_command_injection("file.txt; rm -rf /"))
  assert_true(detect_command_injection("$(cat /etc/passwd)"))
  assert_true(detect_command_injection("normal | malicious"))
  assert_false(detect_command_injection("normal_filename.txt"))
  
  // 安全扫描报告
  let generate_security_scan_report = fn(inputs) {
    let mut vulnerabilities = []
    
    for input in inputs {
      let mut vulns = []
      
      if detect_sql_injection(input) {
        vulns.push("SQL Injection")
      }
      
      if detect_xss(input) {
        vulns.push("Cross-Site Scripting (XSS)")
      }
      
      if detect_path_traversal(input) {
        vulns.push("Path Traversal")
      }
      
      if detect_command_injection(input) {
        vulns.push("Command Injection")
      }
      
      if !vulns.is_empty() {
        vulnerabilities.push({
          input: input,
          vulnerabilities: vulns
        })
      }
    }
    
    {
      total_inputs: inputs.length(),
      vulnerable_inputs: vulnerabilities.length(),
      safe_inputs: inputs.length() - vulnerabilities.length(),
      vulnerabilities: vulnerabilities
    }
  }
  
  let test_inputs = [
    "normal input",
    "'; DROP TABLE users; --",
    "<script>alert('XSS')</script>",
    "../../../etc/passwd",
    "file.txt; rm -rf /",
    "another safe input"
  ]
  
  let report = generate_security_scan_report(test_inputs)
  
  assert_eq(report.total_inputs, 5)
  assert_eq(report.vulnerable_inputs, 4)
  assert_eq(report.safe_inputs, 1)
  assert_eq(report.vulnerabilities.length(), 4)
}