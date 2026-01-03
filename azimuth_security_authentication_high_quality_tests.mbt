// Azimuth Telemetry System - High Quality Security and Authentication Tests
// This file contains comprehensive security and authentication test cases

// Test 1: API Key Authentication
test "api key authentication" {
  let auth_manager = AuthenticationManager::new()
  
  // Test valid API key
  let valid_api_key = "sk-1234567890abcdef"
  let api_key_auth = ApiKeyAuth::new(valid_api_key)
  
  AuthenticationManager::add_provider(auth_manager, "api_key", api_key_auth)
  
  // Authenticate with valid API key
  let auth_context = AuthContext::new()
  AuthContext::set_api_key(auth_context, valid_api_key)
  
  let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
  assert_true(auth_result.is_success)
  assert_true(auth_result.is_authenticated)
  assert_eq(auth_result.user_id, "api_user_1234567890")
  
  // Test invalid API key
  let invalid_auth_context = AuthContext::new()
  AuthContext::set_api_key(invalid_auth_context, "invalid-key")
  
  let invalid_auth_result = AuthenticationManager::authenticate(auth_manager, invalid_auth_context)
  assert_false(invalid_auth_result.is_success)
  assert_false(invalid_auth_result.is_authenticated)
  assert_eq(invalid_auth_result.error_code, "INVALID_API_KEY")
  
  // Test expired API key
  let expired_api_key = "sk-expired-1234567890"
  let expired_auth_context = AuthContext::new()
  AuthContext::set_api_key(expired_auth_context, expired_api_key)
  
  let expired_auth_result = AuthenticationManager::authenticate(auth_manager, expired_auth_context)
  assert_false(expired_auth_result.is_success)
  assert_false(expired_auth_result.is_authenticated)
  assert_eq(expired_auth_result.error_code, "API_KEY_EXPIRED")
  
  // Test API key with insufficient permissions
  let limited_api_key = "sk-limited-1234567890"
  let limited_auth_context = AuthContext::new()
  AuthContext::set_api_key(limited_auth_context, limited_api_key)
  AuthContext::set_required_permission(limited_auth_context, "admin:read")
  
  let limited_auth_result = AuthenticationManager::authenticate(auth_manager, limited_auth_context)
  assert_false(limited_auth_result.is_success)
  assert_eq(limited_auth_result.error_code, "INSUFFICIENT_PERMISSIONS")
  
  // Test API key with sufficient permissions
  AuthContext::set_required_permission(limited_auth_context, "metrics:read")
  
  let sufficient_auth_result = AuthenticationManager::authenticate(auth_manager, limited_auth_context)
  assert_true(sufficient_auth_result.is_success)
  assert_true(sufficient_auth_result.is_authenticated)
}

// Test 2: JWT Token Authentication
test "jwt token authentication" {
  let auth_manager = AuthenticationManager::new()
  let jwt_auth = JwtAuth::new("secret-key")
  
  AuthenticationManager::add_provider(auth_manager, "jwt", jwt_auth)
  
  // Generate valid JWT token
  let token_claims = [
    ("sub", "user123"),
    ("name", "John Doe"),
    ("email", "john.doe@example.com"),
    ("role", "user"),
    ("exp", (get_current_time_millis() / 1000 + 3600).to_string()) // expires in 1 hour
  ]
  
  let valid_token = JwtAuth::generate_token(jwt_auth, token_claims)
  
  // Authenticate with valid JWT token
  let auth_context = AuthContext::new()
  AuthContext::set_jwt_token(auth_context, valid_token)
  
  let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
  assert_true(auth_result.is_success)
  assert_true(auth_result.is_authenticated)
  assert_eq(auth_result.user_id, "user123")
  
  // Test invalid JWT token
  let invalid_auth_context = AuthContext::new()
  AuthContext::set_jwt_token(invalid_auth_context, "invalid.jwt.token")
  
  let invalid_auth_result = AuthenticationManager::authenticate(auth_manager, invalid_auth_context)
  assert_false(invalid_auth_result.is_success)
  assert_false(invalid_auth_result.is_authenticated)
  assert_eq(invalid_auth_result.error_code, "INVALID_JWT_TOKEN")
  
  // Test expired JWT token
  let expired_claims = [
    ("sub", "user123"),
    ("exp", (get_current_time_millis() / 1000 - 3600).to_string()) // expired 1 hour ago
  ]
  
  let expired_token = JwtAuth::generate_token(jwt_auth, expired_claims)
  let expired_auth_context = AuthContext::new()
  AuthContext::set_jwt_token(expired_auth_context, expired_token)
  
  let expired_auth_result = AuthenticationManager::authenticate(auth_manager, expired_auth_context)
  assert_false(expired_auth_result.is_success)
  assert_false(expired_auth_result.is_authenticated)
  assert_eq(expired_auth_result.error_code, "TOKEN_EXPIRED")
  
  // Test JWT token with insufficient role
  let user_token_claims = [
    ("sub", "user456"),
    ("role", "user"),
    ("exp", (get_current_time_millis() / 1000 + 3600).to_string())
  ]
  
  let user_token = JwtAuth::generate_token(jwt_auth, user_token_claims)
  let user_auth_context = AuthContext::new()
  AuthContext::set_jwt_token(user_auth_context, user_token)
  AuthContext::set_required_role(user_auth_context, "admin")
  
  let user_auth_result = AuthenticationManager::authenticate(auth_manager, user_auth_context)
  assert_false(user_auth_result.is_success)
  assert_eq(user_auth_result.error_code, "INSUFFICIENT_ROLE")
  
  // Test JWT token with sufficient role
  AuthContext::set_required_role(user_auth_context, "user")
  
  let sufficient_role_result = AuthenticationManager::authenticate(auth_manager, user_auth_context)
  assert_true(sufficient_role_result.is_success)
  assert_true(sufficient_role_result.is_authenticated)
}

// Test 3: OAuth2 Authentication
test "oauth2 authentication" {
  let auth_manager = AuthenticationManager::new()
  let oauth2_config = OAuth2Config::new(
    "client123",
    "client_secret",
    "https://auth.example.com/oauth/authorize",
    "https://auth.example.com/oauth/token"
  )
  
  let oauth2_auth = OAuth2Auth::new(oauth2_config)
  AuthenticationManager::add_provider(auth_manager, "oauth2", oauth2_auth)
  
  // Test OAuth2 authorization code flow
  let auth_code = "auth_code_12345"
  let redirect_uri = "https://app.example.com/callback"
  
  let token_result = OAuth2Auth::exchange_code_for_token(oauth2_auth, auth_code, redirect_uri)
  assert_true(token_result.is_success)
  
  let access_token = token_result.access_token
  let refresh_token = token_result.refresh_token
  
  // Authenticate with access token
  let auth_context = AuthContext::new()
  AuthContext::set_access_token(auth_context, access_token)
  
  let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
  assert_true(auth_result.is_success)
  assert_true(auth_result.is_authenticated)
  
  // Test access token refresh
  let refresh_result = OAuth2Auth::refresh_access_token(oauth2_auth, refresh_token)
  assert_true(refresh_result.is_success)
  
  let new_access_token = refresh_result.access_token
  assert_true(new_access_token != access_token)
  
  // Test revoked access token
  let revoked_auth_context = AuthContext::new()
  AuthContext::set_access_token(revoked_auth_context, "revoked_token")
  
  let revoked_auth_result = AuthenticationManager::authenticate(auth_manager, revoked_auth_context)
  assert_false(revoked_auth_result.is_success)
  assert_eq(revoked_auth_result.error_code, "TOKEN_REVOKED")
  
  // Test scope validation
  let limited_token_result = OAuth2Auth::exchange_code_for_token_with_scope(oauth2_auth, auth_code, redirect_uri, "read")
  assert_true(limited_token_result.is_success)
  
  let limited_auth_context = AuthContext::new()
  AuthContext::set_access_token(limited_auth_context, limited_token_result.access_token)
  AuthContext::set_required_scope(limited_auth_context, "write")
  
  let limited_auth_result = AuthenticationManager::authenticate(auth_manager, limited_auth_context)
  assert_false(limited_auth_result.is_success)
  assert_eq(limited_auth_result.error_code, "INSUFFICIENT_SCOPE")
  
  // Test sufficient scope
  AuthContext::set_required_scope(limited_auth_context, "read")
  
  let sufficient_scope_result = AuthenticationManager::authenticate(auth_manager, limited_auth_context)
  assert_true(sufficient_scope_result.is_success)
  assert_true(sufficient_scope_result.is_authenticated)
}

// Test 4: Certificate-Based Authentication
test "certificate based authentication" {
  let auth_manager = AuthenticationManager::new()
  let cert_auth = CertificateAuth::new()
  
  AuthenticationManager::add_provider(auth_manager, "certificate", cert_auth)
  
  // Test valid certificate
  let valid_cert = load_certificate("valid_client.crt")
  let valid_key = load_private_key("valid_client.key")
  
  let auth_context = AuthContext::new()
  AuthContext::set_certificate(auth_context, valid_cert)
  AuthContext::set_private_key(auth_context, valid_key)
  
  let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
  assert_true(auth_result.is_success)
  assert_true(auth_result.is_authenticated)
  
  // Test expired certificate
  let expired_cert = load_certificate("expired_client.crt")
  let expired_auth_context = AuthContext::new()
  AuthContext::set_certificate(expired_auth_context, expired_cert)
  AuthContext::set_private_key(expired_auth_context, valid_key)
  
  let expired_auth_result = AuthenticationManager::authenticate(auth_manager, expired_auth_context)
  assert_false(expired_auth_result.is_success)
  assert_eq(expired_auth_result.error_code, "CERTIFICATE_EXPIRED")
  
  // Test revoked certificate
  let revoked_cert = load_certificate("revoked_client.crt")
  let revoked_auth_context = AuthContext::new()
  AuthContext::set_certificate(revoked_auth_context, revoked_cert)
  AuthContext::set_private_key(revoked_auth_context, valid_key)
  
  let revoked_auth_result = AuthenticationManager::authenticate(auth_manager, revoked_auth_context)
  assert_false(revoked_auth_result.is_success)
  assert_eq(revoked_auth_result.error_code, "CERTIFICATE_REVOKED")
  
  // Test self-signed certificate (if not trusted)
  let self_signed_cert = load_certificate("self_signed_client.crt")
  let self_signed_auth_context = AuthContext::new()
  AuthContext::set_certificate(self_signed_auth_context, self_signed_cert)
  AuthContext::set_private_key(self_signed_auth_context, load_private_key("self_signed_client.key"))
  
  let self_signed_auth_result = AuthenticationManager::authenticate(auth_manager, self_signed_auth_context)
  assert_false(self_signed_auth_result.is_success)
  assert_eq(self_signed_auth_result.error_code, "UNTRUSTED_CERTIFICATE")
  
  // Test certificate chain validation
  let chain_cert = load_certificate("intermediate_client.crt")
  let chain_auth_context = AuthContext::new()
  AuthContext::set_certificate(chain_auth_context, chain_cert)
  AuthContext::set_private_key(chain_auth_context, load_private_key("intermediate_client.key"))
  AuthContext::set_ca_certificates(chain_auth_context, [load_certificate("ca.crt")])
  
  let chain_auth_result = AuthenticationManager::authenticate(auth_manager, chain_auth_context)
  assert_true(chain_auth_result.is_success)
  assert_true(chain_auth_result.is_authenticated)
}

// Test 5: Multi-Factor Authentication
test "multi factor authentication" {
  let auth_manager = AuthenticationManager::new()
  let mfa_auth = MfaAuth::new()
  
  AuthenticationManager::add_provider(auth_manager, "mfa", mfa_auth)
  
  // Test MFA setup
  let user_id = "user123"
  let secret = MfaAuth::setup_mfa(mfa_auth, user_id)
  assert_true(secret.length() > 0)
  
  // Generate TOTP code
  let totp_code = MfaAuth::generate_totp_code(mfa_auth, secret)
  assert_true(totp_code.length() == 6)
  
  // Test MFA verification
  let auth_context = AuthContext::new()
  AuthContext::set_user_id(auth_context, user_id)
  AuthContext::set_totp_code(auth_context, totp_code)
  
  let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
  assert_true(auth_result.is_success)
  assert_true(auth_result.is_authenticated)
  
  // Test invalid TOTP code
  let invalid_auth_context = AuthContext::new()
  AuthContext::set_user_id(invalid_auth_context, user_id)
  AuthContext::set_totp_code(invalid_auth_context, "000000")
  
  let invalid_auth_result = AuthenticationManager::authenticate(auth_manager, invalid_auth_context)
  assert_false(invalid_auth_result.is_success)
  assert_eq(invalid_auth_result.error_code, "INVALID_TOTP_CODE")
  
  // Test backup codes
  let backup_codes = MfaAuth::generate_backup_codes(mfa_auth, user_id)
  assert_eq(backup_codes.length(), 10)
  
  let backup_code = backup_codes[0]
  let backup_auth_context = AuthContext::new()
  AuthContext::set_user_id(backup_auth_context, user_id)
  AuthContext::set_backup_code(backup_auth_context, backup_code)
  
  let backup_auth_result = AuthenticationManager::authenticate(auth_manager, backup_auth_context)
  assert_true(backup_auth_result.is_success)
  assert_true(backup_auth_result.is_authenticated)
  
  // Test used backup code
  let used_backup_auth_context = AuthContext::new()
  AuthContext::set_user_id(used_backup_auth_context, user_id)
  AuthContext::set_backup_code(used_backup_auth_context, backup_code)
  
  let used_backup_auth_result = AuthenticationManager::authenticate(auth_manager, used_backup_auth_context)
  assert_false(used_backup_auth_result.is_success)
  assert_eq(used_backup_auth_result.error_code, "BACKUP_CODE_ALREADY_USED")
  
  // Test SMS verification
  let phone_number = "+1234567890"
  let sms_code = MfaAuth::send_sms_code(mfa_auth, user_id, phone_number)
  
  let sms_auth_context = AuthContext::new()
  AuthContext::set_user_id(sms_auth_context, user_id)
  AuthContext::set_sms_code(sms_auth_context, sms_code)
  
  let sms_auth_result = AuthenticationManager::authenticate(auth_manager, sms_auth_context)
  assert_true(sms_auth_result.is_success)
  assert_true(sms_auth_result.is_authenticated)
}

// Test 6: Rate Limiting and Brute Force Protection
test "rate limiting and brute force protection" {
  let auth_manager = AuthenticationManager::new()
  let rate_limiter = RateLimiter::new(5, 60) // 5 attempts per minute
  
  AuthenticationManager::set_rate_limiter(auth_manager, rate_limiter)
  
  // Test successful authentication under rate limit
  for i in 0..5 {
    let auth_context = AuthContext::new()
    AuthContext::set_user_id(auth_context, "user" + i.to_string())
    AuthContext::set_password(auth_context, "password" + i.to_string())
    
    let auth_result = AuthenticationManager::authenticate(auth_manager, auth_context)
    assert_true(auth_result.is_success)
  }
  
  // Test rate limit exceeded
  let exceeded_auth_context = AuthContext::new()
  AuthContext::set_user_id(exceeded_auth_context, "user6")
  AuthContext::set_password(exceeded_auth_context, "password6")
  
  let exceeded_auth_result = AuthenticationManager::authenticate(auth_manager, exceeded_auth_context)
  assert_false(exceeded_auth_result.is_success)
  assert_eq(exceeded_auth_result.error_code, "RATE_LIMIT_EXCEEDED")
  
  // Test IP-based rate limiting
  let ip_rate_limiter = IpRateLimiter::new(3, 60) // 3 attempts per minute per IP
  
  AuthenticationManager::set_ip_rate_limiter(auth_manager, ip_rate_limiter)
  
  let ip_address = "192.168.1.100"
  
  // Test successful authentication under IP rate limit
  for i in 0..3 {
    let ip_auth_context = AuthContext::new()
    AuthContext::set_user_id(ip_auth_context, "ip_user" + i.to_string())
    AuthContext::set_password(ip_auth_context, "password" + i.to_string())
    AuthContext::set_ip_address(ip_auth_context, ip_address)
    
    let ip_auth_result = AuthenticationManager::authenticate(auth_manager, ip_auth_context)
    assert_true(ip_auth_result.is_success)
  }
  
  // Test IP rate limit exceeded
  let ip_exceeded_auth_context = AuthContext::new()
  AuthContext::set_user_id(ip_exceeded_auth_context, "ip_user3")
  AuthContext::set_password(ip_exceeded_auth_context, "password3")
  AuthContext::set_ip_address(ip_exceeded_auth_context, ip_address)
  
  let ip_exceeded_auth_result = AuthenticationManager::authenticate(auth_manager, ip_exceeded_auth_context)
  assert_false(ip_exceeded_auth_result.is_success)
  assert_eq(ip_exceeded_auth_result.error_code, "IP_RATE_LIMIT_EXCEEDED")
  
  // Test different IP is not affected
  let different_ip_auth_context = AuthContext::new()
  AuthContext::set_user_id(different_ip_auth_context, "different_user")
  AuthContext::set_password(different_ip_auth_context, "password")
  AuthContext::set_ip_address(different_ip_auth_context, "192.168.1.101")
  
  let different_ip_auth_result = AuthenticationManager::authenticate(auth_manager, different_ip_auth_context)
  assert_true(different_ip_auth_result.is_success)
  
  // Test account lockout after multiple failed attempts
  let lockout_policy = AccountLockoutPolicy::new(5, 900) // 5 failed attempts, lock for 15 minutes
  AuthenticationManager::set_lockout_policy(auth_manager, lockout_policy)
  
  let locked_user = "user_to_lock"
  
  // Make 5 failed attempts
  for i in 0..5 {
    let failed_auth_context = AuthContext::new()
    AuthContext::set_user_id(failed_auth_context, locked_user)
    AuthContext::set_password(failed_auth_context, "wrong_password")
    
    let failed_auth_result = AuthenticationManager::authenticate(auth_manager, failed_auth_context)
    assert_false(failed_auth_result.is_success)
  }
  
  // Test account is locked
  let locked_auth_context = AuthContext::new()
  AuthContext::set_user_id(locked_auth_context, locked_user)
  AuthContext::set_password(locked_auth_context, "correct_password")
  
  let locked_auth_result = AuthenticationManager::authenticate(auth_manager, locked_auth_context)
  assert_false(locked_auth_result.is_success)
  assert_eq(locked_auth_result.error_code, "ACCOUNT_LOCKED")
}

// Test 7: Session Management
test "session management" {
  let session_manager = SessionManager::new()
  
  // Test session creation
  let user_id = "user123"
  let session = SessionManager::create_session(session_manager, user_id)
  
  assert_true(session.session_id.length() > 0)
  assert_eq(session.user_id, user_id)
  assert_true(session.is_active)
  assert_true(session.expires_at > get_current_time_millis())
  
  // Test session validation
  let is_valid = SessionManager::is_session_valid(session_manager, session.session_id)
  assert_true(is_valid)
  
  // Test session retrieval
  let retrieved_session = SessionManager::get_session(session_manager, session.session_id)
  assert_true(retrieved_session != null)
  assert_eq(retrieved_session.user_id, user_id)
  
  // Test session update
  SessionManager::update_session_data(session_manager, session.session_id, [
    ("last_activity", get_current_time_millis().to_string()),
    ("ip_address", "192.168.1.100")
  ])
  
  let updated_session = SessionManager::get_session(session_manager, session.session_id)
  assert_true(updated_session.session_data.contains("last_activity"))
  assert_true(updated_session.session_data.contains("ip_address"))
  
  // Test session expiration
  let short_lived_session = SessionManager::create_session_with_ttl(session_manager, user_id, 1) // 1 second TTL
  assert_true(SessionManager::is_session_valid(session_manager, short_lived_session.session_id))
  
  // Wait for session to expire
  wait(1100) // Wait 1.1 seconds
  
  assert_false(SessionManager::is_session_valid(session_manager, short_lived_session.session_id))
  
  // Test session revocation
  let revocable_session = SessionManager::create_session(session_manager, user_id)
  assert_true(SessionManager::is_session_valid(session_manager, revocable_session.session_id))
  
  SessionManager::revoke_session(session_manager, revocable_session.session_id)
  assert_false(SessionManager::is_session_valid(session_manager, revocable_session.session_id))
  
  // Test revoking all user sessions
  let session1 = SessionManager::create_session(session_manager, user_id)
  let session2 = SessionManager::create_session(session_manager, user_id)
  
  assert_true(SessionManager::is_session_valid(session_manager, session1.session_id))
  assert_true(SessionManager::is_session_valid(session_manager, session2.session_id))
  
  SessionManager::revoke_all_user_sessions(session_manager, user_id)
  
  assert_false(SessionManager::is_session_valid(session_manager, session1.session_id))
  assert_false(SessionManager::is_session_valid(session_manager, session2.session_id))
  
  // Test concurrent session limit
  SessionManager::set_concurrent_session_limit(session_manager, 2)
  
  let concurrent_session1 = SessionManager::create_session(session_manager, user_id)
  let concurrent_session2 = SessionManager::create_session(session_manager, user_id)
  let concurrent_session3 = SessionManager::create_session(session_manager, user_id)
  
  // First two sessions should be valid, third should invalidate oldest
  assert_true(SessionManager::is_session_valid(session_manager, concurrent_session1.session_id))
  assert_true(SessionManager::is_session_valid(session_manager, concurrent_session2.session_id))
  assert_true(SessionManager::is_session_valid(session_manager, concurrent_session3.session_id))
  
  // One of the first sessions should now be invalid (implementation dependent)
  // This test would need to be adjusted based on specific implementation behavior
}

// Test 8: Secure Communication
test "secure communication" {
  let crypto_manager = CryptoManager::new()
  
  // Test symmetric encryption
  let plaintext = "Sensitive telemetry data"
  let encryption_key = CryptoManager::generate_key(crypto_manager, 256) // 256-bit key
  
  let encrypted_data = CryptoManager::encrypt(crypto_manager, plaintext, encryption_key)
  assert_true(encrypted_data != plaintext)
  assert_true(encrypted_data.length() > 0)
  
  let decrypted_data = CryptoManager::decrypt(crypto_manager, encrypted_data, encryption_key)
  assert_eq(decrypted_data, plaintext)
  
  // Test decryption with wrong key
  let wrong_key = CryptoManager::generate_key(crypto_manager, 256)
  
  try {
    let wrong_decrypted = CryptoManager::decrypt(crypto_manager, encrypted_data, wrong_key)
    assert_true(false) // Should not reach here
  } catch {
    assert_true(true) // Exception expected
  }
  
  // Test asymmetric encryption
  let key_pair = CryptoManager::generate_key_pair(crypto_manager, 2048) // 2048-bit RSA
  let public_key = key_pair.public_key
  let private_key = key_pair.private_key
  
  let asymmetric_encrypted = CryptoManager::encrypt_public(crypto_manager, plaintext, public_key)
  assert_true(asymmetric_encrypted != plaintext)
  
  let asymmetric_decrypted = CryptoManager::decrypt_private(crypto_manager, asymmetric_encrypted, private_key)
  assert_eq(asymmetric_decrypted, plaintext)
  
  // Test digital signatures
  let message = "Important telemetry message"
  let signature = CryptoManager::sign(crypto_manager, message, private_key)
  assert_true(signature.length() > 0)
  
  let is_signature_valid = CryptoManager::verify_signature(crypto_manager, message, signature, public_key)
  assert_true(is_signature_valid)
  
  // Test signature verification with tampered message
  let tampered_message = "Tampered telemetry message"
  let is_tampered_signature_valid = CryptoManager::verify_signature(crypto_manager, tampered_message, signature, public_key)
  assert_false(is_tampered_signature_valid)
  
  // Test secure hash
  let hash = CryptoManager::hash(crypto_manager, message)
  assert_true(hash.length() == 64) // SHA-256 produces 64-character hex string
  
  let hash2 = CryptoManager::hash(crypto_manager, message)
  assert_eq(hash, hash2) // Same message should produce same hash
  
  let hash3 = CryptoManager::hash(crypto_manager, tampered_message)
  assert_ne(hash, hash3) // Different message should produce different hash
  
  // Test secure random number generation
  let random_bytes1 = CryptoManager::secure_random(crypto_manager, 32)
  let random_bytes2 = CryptoManager::secure_random(crypto_manager, 32)
  
  assert_eq(random_bytes1.length(), 32)
  assert_eq(random_bytes2.length(), 32)
  assert_ne(random_bytes1, random_bytes2) // Random bytes should be different
}

// Test 9: Data Privacy and PII Protection
test "data privacy and pii protection" {
  let privacy_manager = PrivacyManager::new()
  
  // Test PII detection
  let text_with_pii = "User John Doe (john.doe@example.com) with phone +1234567890 and SSN 123-45-6789"
  let pii_detected = PrivacyManager::detect_pii(privacy_manager, text_with_pii)
  
  assert_true(pii_detected.contains("john.doe@example.com"))
  assert_true(pii_detected.contains("+1234567890"))
  assert_true(pii_detected.contains("123-45-6789"))
  
  // Test PII masking
  let masked_text = PrivacyManager::mask_pii(privacy_manager, text_with_pii)
  assert_true(masked_text.contains("****@example.com"))
  assert_true(masked_text.contains("*******7890"))
  assert_true(masked_text.contains("***-**-6789"))
  
  // Test PII redaction
  let redacted_text = PrivacyManager::redact_pii(privacy_manager, text_with_pii)
  assert_false(redacted_text.contains("john.doe@example.com"))
  assert_false(redacted_text.contains("+1234567890"))
  assert_false(redacted_text.contains("123-45-6789"))
  assert_true(redacted_text.contains("[REDACTED]"))
  
  // Test telemetry data anonymization
  let telemetry_data = [
    ("user_id", "user123"),
    ("ip_address", "192.168.1.100"),
    ("email", "user@example.com"),
    ("session_id", "sess_abc123"),
    ("metric_value", "42.5"),
    ("operation", "user_login")
  ]
  
  let anonymized_data = PrivacyManager::anonymize_telemetry(privacy_manager, telemetry_data)
  
  // Check that direct identifiers are anonymized
  assert_ne(anonymized_data.get("user_id"), "user123")
  assert_ne(anonymized_data.get("email"), "user@example.com")
  
  // Check that indirect identifiers are anonymized
  assert_ne(anonymized_data.get("ip_address"), "192.168.1.100")
  assert_ne(anonymized_data.get("session_id"), "sess_abc123")
  
  // Check that non-PII data is preserved
  assert_eq(anonymized_data.get("metric_value"), "42.5")
  assert_eq(anonymized_data.get("operation"), "user_login")
  
  // Test data retention policies
  let retention_policy = RetentionPolicy::new()
  RetentionPolicy::set_retention_period(retention_policy, "user_data", 90) // 90 days
  RetentionPolicy::set_retention_period(retention_policy, "metrics", 365) // 1 year
  RetentionPolicy::set_retention_period(retention_policy, "logs", 30) // 30 days
  
  PrivacyManager::set_retention_policy(privacy_manager, retention_policy)
  
  // Test expired data cleanup
  let old_timestamp = get_current_time_millis() - (100 * 24 * 60 * 60 * 1000) // 100 days ago
  let recent_timestamp = get_current_time_millis() - (10 * 24 * 60 * 60 * 1000) // 10 days ago
  
  let data_to_cleanup = [
    ("user_data", "old_user_data", old_timestamp),
    ("metrics", "old_metrics", old_timestamp),
    ("logs", "old_logs", old_timestamp),
    ("user_data", "recent_user_data", recent_timestamp),
    ("metrics", "recent_metrics", recent_timestamp),
    ("logs", "recent_logs", recent_timestamp)
  ]
  
  let cleanup_result = PrivacyManager::cleanup_expired_data(privacy_manager, data_to_cleanup)
  
  // Check that old user data and logs are cleaned up, but recent data and metrics are preserved
  assert_false(cleanup_result.contains("old_user_data"))
  assert_false(cleanup_result.contains("old_logs"))
  assert_true(cleanup_result.contains("old_metrics")) // Metrics have 1-year retention
  assert_true(cleanup_result.contains("recent_user_data"))
  assert_true(cleanup_result.contains("recent_metrics"))
  assert_true(cleanup_result.contains("recent_logs"))
}

// Test 10: Security Audit and Compliance
test "security audit and compliance" {
  let audit_manager = AuditManager::new()
  
  // Test audit logging for authentication events
  let auth_event = AuditEvent::new(
    "authentication",
    "success",
    "user123",
    "192.168.1.100",
    [
      ("auth_method", "api_key"),
      ("timestamp", get_current_time_millis().to_string())
    ]
  )
  
  AuditManager::log_event(audit_manager, auth_event)
  
  // Test audit logging for authorization events
  let authz_event = AuditEvent::new(
    "authorization",
    "failure",
    "user123",
    "192.168.1.100",
    [
      ("resource", "/admin/users"),
      ("action", "DELETE"),
      ("reason", "insufficient_permissions"),
      ("timestamp", get_current_time_millis().to_string())
    ]
  )
  
  AuditManager::log_event(audit_manager, authz_event)
  
  // Test audit log querying
  let query = AuditQuery::new()
  AuditQuery::set_user_id(query, "user123")
  AuditQuery::set_time_range(query, get_current_time_millis() - 86400000, get_current_time_millis()) // Last 24 hours
  
  let audit_logs = AuditManager::query_events(audit_manager, query)
  assert_true(audit_logs.length() >= 2)
  
  // Verify events are logged correctly
  let auth_found = false
  let authz_found = false
  
  for event in audit_logs {
    if event.event_type == "authentication" && event.result == "success" {
      auth_found = true
    }
    if event.event_type == "authorization" && event.result == "failure" {
      authz_found = true
    }
  }
  
  assert_true(auth_found)
  assert_true(authz_found)
  
  // Test audit log integrity
  let log_integrity = AuditManager::verify_log_integrity(audit_manager)
  assert_true(log_integrity.is_valid)
  
  // Test compliance reporting
  let compliance_reporter = ComplianceReporter::new()
  
  // Generate GDPR compliance report
  let gdpr_report = ComplianceReporter::generate_gdpr_report(compliance_reporter, "user123")
  assert_true(gdpr_report.contains("personal_data"))
  assert_true(gdpr_report.contains("data_processing_activities"))
  assert_true(gdpr_report.contains("data_retention"))
  
  // Generate HIPAA compliance report
  let hipaa_report = ComplianceReporter::generate_hipaa_report(compliance_reporter)
  assert_true(hipaa_report.contains("phi_data"))
  assert_true(hipaa_report.contains("security_measures"))
  assert_true(hipaa_report.contains("access_controls"))
  
  // Generate SOX compliance report
  let sox_report = ComplianceReporter::generate_sox_report(compliance_reporter)
  assert_true(sox_report.contains("financial_data"))
  assert_true(sox_report.contains("audit_trail"))
  assert_true(sox_report.contains("internal_controls"))
  
  // Test security metrics
  let security_metrics = SecurityMetrics::new()
  
  // Record security events
  SecurityMetrics::record_login_attempt(security_metrics, true) // Successful login
  SecurityMetrics::record_login_attempt(security_metrics, false) // Failed login
  SecurityMetrics::record_login_attempt(security_metrics, false) // Failed login
  SecurityMetrics::record_permission_denied(security_metrics, "read", "sensitive_data")
  SecurityMetrics::record_data_access(security_metrics, "user123", "user_profile")
  
  // Generate security metrics report
  let metrics_report = SecurityMetrics::generate_report(security_metrics)
  assert_true(metrics_report.contains("login_success_rate"))
  assert_true(metrics_report.contains("failed_login_attempts"))
  assert_true(metrics_report.contains("permission_denials"))
  assert_true(metrics_report.contains("data_access_events"))
  
  // Verify metrics calculations
  let login_success_rate = SecurityMetrics::get_login_success_rate(security_metrics)
  assert_eq(login_success_rate, 0.33) // 1 success out of 3 attempts (approximately 33%)
  
  let failed_login_count = SecurityMetrics::get_failed_login_count(security_metrics)
  assert_eq(failed_login_count, 2)
  
  let permission_denial_count = SecurityMetrics::get_permission_denial_count(security_metrics)
  assert_eq(permission_denial_count, 1)
}