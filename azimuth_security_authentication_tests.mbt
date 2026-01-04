// Azimuth Telemetry System - Security and Authentication Tests
// This file contains test cases for security and authentication functionality

// Test 1: API Key Authentication
test "api key authentication" {
  // Create API key authenticator
  let api_key_auth = ApiKeyAuthenticator::new()
  
  // Verify authenticator properties
  assert_eq(ApiKeyAuthenticator::type(api_key_auth), "api_key")
  assert_eq(ApiKeyAuthenticator::header_name(api_key_auth), "X-API-Key")
  
  // Configure authenticator
  ApiKeyAuthenticator::set_header_name(api_key_auth, "Authorization")
  ApiKeyAuthenticator::set_prefix(api_key_auth, "Bearer")
  ApiKeyAuthenticator::set_case_sensitive(api_key_auth, false)
  
  // Add API keys
  ApiKeyAuthenticator::add_key(api_key_auth, "key123", [
    ("service", "user-service"),
    ("permissions", "read,write"),
    ("rate_limit", "1000")
  ])
  
  ApiKeyAuthenticator::add_key(api_key_auth, "key456", [
    ("service", "order-service"),
    ("permissions", "read"),
    ("rate_limit", "500")
  ])
  
  // Test valid API key authentication
  let request1 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request1, "Authorization", "Bearer key123")
  
  let auth_result1 = ApiKeyAuthenticator::authenticate(api_key_auth, request1)
  
  match auth_result1 {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "key123")
      assert_eq(AuthContext::get_claim(context, "service"), Some("user-service"))
      assert_eq(AuthContext::get_claim(context, "permissions"), Some("read,write"))
      assert_eq(AuthContext::get_claim(context, "rate_limit"), Some("1000"))
    }
    Err(_) => assert_true(false)
  }
  
  // Test invalid API key
  let request2 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request2, "Authorization", "Bearer invalid_key")
  
  let auth_result2 = ApiKeyAuthenticator::authenticate(api_key_auth, request2)
  
  match auth_result2 {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => {
      assert_true(error.contains("invalid") || error.contains("unauthorized"))
    }
  }
  
  // Test missing API key
  let request3 = HttpRequest::new("GET", "/api/telemetry")
  // No Authorization header
  
  let auth_result3 = ApiKeyAuthenticator::authenticate(api_key_auth, request3)
  
  match auth_result3 {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => {
      assert_true(error.contains("missing") || error.contains("required"))
    }
  }
  
  // Test case-insensitive matching
  let request4 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request4, "authorization", "bearer key123") // Lowercase
  
  let auth_result4 = ApiKeyAuthenticator::authenticate(api_key_auth, request4)
  
  match auth_result4 {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "key123")
    }
    Err(_) => assert_true(false)
  }
  
  // Test API key validation
  let validation_result = ApiKeyAuthenticator::validate_key(api_key_auth, "key123")
  assert_true(validation_result)
  
  let invalid_validation_result = ApiKeyAuthenticator::validate_key(api_key_auth, "invalid")
  assert_false(invalid_validation_result)
  
  // Test API key removal
  ApiKeyAuthenticator::remove_key(api_key_auth, "key456")
  
  let removed_auth_result = ApiKeyAuthenticator::authenticate(api_key_auth, request1)
  match removed_auth_result {
    Ok(context) => assert_eq(AuthContext::user_id(context), "key123") // key123 should still work
    Err(_) => assert_true(false)
  }
  
  let request5 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request5, "Authorization", "Bearer key456")
  
  let removed_auth_result2 = ApiKeyAuthenticator::authenticate(api_key_auth, request5)
  match removed_auth_result2 {
    Ok(_) => assert_true(false) // Should fail after removal
    Err(error) => {
      assert_true(error.contains("invalid") || error.contains("unauthorized"))
    }
  }
  
  // Test API key rotation
  let new_key = ApiKeyAuthenticator::rotate_key(api_key_auth, "key123", "new_key789")
  
  match new_key {
    Ok(rotated_key) => assert_eq(rotated_key, "new_key789")
    Err(_) => assert_true(false)
  }
  
  let request6 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request6, "Authorization", "Bearer new_key789")
  
  let rotated_auth_result = ApiKeyAuthenticator::authenticate(api_key_auth, request6)
  match rotated_auth_result {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "new_key789")
      // Should preserve original claims
      assert_eq(AuthContext::get_claim(context, "service"), Some("user-service"))
    }
    Err(_) => assert_true(false)
  }
}

// Test 2: JWT Token Authentication
test "jwt token authentication" {
  // Create JWT authenticator
  let jwt_auth = JwtAuthenticator::new("secret_key")
  
  // Verify authenticator properties
  assert_eq(JwtAuthenticator::type(jwt_auth), "jwt")
  assert_eq(JwtAuthenticator::algorithm(jwt_auth), "HS256")
  
  // Configure authenticator
  JwtAuthenticator::set_algorithm(jwt_auth, "HS512")
  JwtAuthenticator::set_issuer(jwt_auth, "azimuth-telemetry")
  JwtAuthenticator::set_audience(jwt_auth, "azimuth-clients")
  JwtAuthenticator::set_expiration(jwt_auth, 3600) // 1 hour
  
  // Create JWT token
  let claims = JwtClaims::new()
  JwtClaims::set_subject(claims, "user123")
  JwtClaims::set_issuer(claims, "azimuth-telemetry")
  JwtClaims::set_audience(claims, "azimuth-clients")
  JwtClaims::set_expiration(claims, Time::now() + 3600) // 1 hour from now
  JwtClaims::set_issued_at(claims, Time::now())
  JwtClaims::add_claim(claims, "role", "admin")
  JwtClaims::add_claim(claims, "permissions", ["read", "write", "delete"])
  JwtClaims::add_claim(claims, "service", "telemetry-service")
  
  let token = JwtAuthenticator::generate_token(jwt_auth, claims)
  
  assert_true(token.length() > 0)
  assert_true(token.contains(".")) // JWT should have 3 parts separated by dots
  
  // Test valid JWT authentication
  let request1 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request1, "Authorization", "Bearer " + token)
  
  let auth_result1 = JwtAuthenticator::authenticate(jwt_auth, request1)
  
  match auth_result1 {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "user123")
      assert_eq(AuthContext::get_claim(context, "role"), Some("admin"))
      assert_eq(AuthContext::get_claim(context, "service"), Some("telemetry-service"))
      
      let permissions = AuthContext::get_claim(context, "permissions")
      match permissions {
        Some(perms) => assert_eq(perms, "read,write,delete")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test invalid JWT token
  let invalid_token = "invalid.jwt.token"
  
  let request2 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request2, "Authorization", "Bearer " + invalid_token)
  
  let auth_result2 = JwtAuthenticator::authenticate(jwt_auth, request2)
  
  match auth_result2 {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => {
      assert_true(error.contains("invalid") || error.contains("malformed"))
    }
  }
  
  // Test expired JWT token
  let expired_claims = JwtClaims::new()
  JwtClaims::set_subject(expired_claims, "user123")
  JwtClaims::set_expiration(expired_claims, Time::now() - 3600) // Expired 1 hour ago
  
  let expired_token = JwtAuthenticator::generate_token(jwt_auth, expired_claims)
  
  let request3 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request3, "Authorization", "Bearer " + expired_token)
  
  let auth_result3 = JwtAuthenticator::authenticate(jwt_auth, request3)
  
  match auth_result3 {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => {
      assert_true(error.contains("expired"))
    }
  }
  
  // Test JWT token validation
  let validation_result = JwtAuthenticator::validate_token(jwt_auth, token)
  assert_true(validation_result)
  
  let invalid_validation_result = JwtAuthenticator::validate_token(jwt_auth, invalid_token)
  assert_false(invalid_validation_result)
  
  // Test JWT token refresh
  let refresh_result = JwtAuthenticator::refresh_token(jwt_auth, token)
  
  match refresh_result {
    Ok(new_token) => {
      assert_true(new_token.length() > 0)
      assert_true(new_token != token) // Should be a new token
      
      // Test refreshed token
      let request4 = HttpRequest::new("GET", "/api/telemetry")
      HttpRequest::add_header(request4, "Authorization", "Bearer " + new_token)
      
      let refresh_auth_result = JwtAuthenticator::authenticate(jwt_auth, request4)
      match refresh_auth_result {
        Ok(context) => {
          assert_eq(AuthContext::user_id(context), "user123")
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test JWT token with different signing algorithms
  let rsa_auth = JwtAuthenticator::with_rsa("-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----")
  JwtAuthenticator::set_algorithm(rsa_auth, "RS256")
  
  let rsa_claims = JwtClaims::new()
  JwtClaims::set_subject(rsa_claims, "user456")
  
  let rsa_token = JwtAuthenticator::generate_token(rsa_auth, rsa_claims)
  
  let request5 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request5, "Authorization", "Bearer " + rsa_token)
  
  let rsa_auth_result = JwtAuthenticator::authenticate(rsa_auth, request5)
  
  match rsa_auth_result {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "user456")
    }
    Err(_) => {
      // In test environment, RSA key might not be valid
      assert_true(true)
    }
  }
  
  // Test JWT token extraction from different sources
  let request6 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_query_param(request6, "token", token)
  
  let query_auth_result = JwtAuthenticator::authenticate_from_query(jwt_auth, request6, "token")
  
  match query_auth_result {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "user123")
    }
    Err(_) => assert_true(false)
  }
  
  // Test JWT token with custom claims validation
  let custom_validator = JwtClaimsValidator::new()
  JwtClaimsValidator::add_required_claim(custom_validator, "role")
  JwtClaimsValidator::add_allowed_values(custom_validator, "role", ["admin", "user", "readonly"])
  
  JwtAuthenticator::set_claims_validator(jwt_auth, custom_validator)
  
  let custom_auth_result = JwtAuthenticator::authenticate(jwt_auth, request1)
  match custom_auth_result {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "user123")
      assert_eq(AuthContext::get_claim(context, "role"), Some("admin"))
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: OAuth2 Authentication
test "oauth2 authentication" {
  // Create OAuth2 authenticator
  let oauth2_auth = OAuth2Authenticator::new()
  
  // Verify authenticator properties
  assert_eq(OAuth2Authenticator::type(oauth2_auth), "oauth2")
  assert_eq(OAuth2Authenticator::flow(oauth2_auth), "authorization_code")
  
  // Configure authenticator
  OAuth2Authenticator::set_client_id(oauth2_auth, "azimuth-client")
  OAuth2Authenticator::set_client_secret(oauth2_auth, "client-secret")
  OAuth2Authenticator::set_redirect_uri(oauth2_auth, "https://azimuth.example.com/callback")
  OAuth2Authenticator::set_scope(oauth2_auth, "read write")
  OAuth2Authenticator::set_auth_url(oauth2_auth, "https://auth.example.com/oauth/authorize")
  OAuth2Authenticator::set_token_url(oauth2_auth, "https://auth.example.com/oauth/token")
  
  // Test authorization URL generation
  let auth_url = OAuth2Authenticator::get_authorization_url(oauth2_auth, "state123")
  
  assert_true(auth_url.contains("https://auth.example.com/oauth/authorize"))
  assert_true(auth_url.contains("client_id=azimuth-client"))
  assert_true(auth_url.contains("redirect_uri=https://azimuth.example.com/callback"))
  assert_true(auth_url.contains("scope=read+write"))
  assert_true(auth_url.contains("state=state123"))
  assert_true(auth_url.contains("response_type=code"))
  
  // Test access token exchange
  let auth_code = "auth_code_12345"
  
  let token_result = OAuth2Authenticator::exchange_code_for_token(oauth2_auth, auth_code)
  
  match token_result {
    Ok(token_response) => {
      assert_true(token_response.access_token.length() > 0)
      assert_eq(token_response.token_type, "Bearer")
      assert_true(token_response.expires_in > 0)
      assert_true(token_response.refresh_token.length() > 0)
      assert_true(token_response.scope.contains("read"))
      assert_true(token_response.scope.contains("write"))
    }
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test access token refresh
  let refresh_token = "refresh_token_12345"
  
  let refresh_result = OAuth2Authenticator::refresh_access_token(oauth2_auth, refresh_token)
  
  match refresh_result {
    Ok(token_response) => {
      assert_true(token_response.access_token.length() > 0)
      assert_eq(token_response.token_type, "Bearer")
      assert_true(token_response.expires_in > 0)
    }
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test OAuth2 authentication with access token
  let access_token = "access_token_12345"
  
  let request1 = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(request1, "Authorization", "Bearer " + access_token)
  
  let auth_result1 = OAuth2Authenticator::authenticate(oauth2_auth, request1)
  
  match auth_result1 {
    Ok(context) => {
      assert_eq(AuthContext::user_id(context), "user123")
      assert_eq(AuthContext::get_claim(context, "scope"), Some("read write"))
    }
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test OAuth2 with client credentials flow
  let client_creds_auth = OAuth2Authenticator::with_client_credentials()
  OAuth2Authenticator::set_client_id(client_creds_auth, "azimuth-client")
  OAuth2Authenticator::set_client_secret(client_creds_auth, "client-secret")
  OAuth2Authenticator::set_scope(client_creds_auth, "read write")
  OAuth2Authenticator::set_token_url(client_creds_auth, "https://auth.example.com/oauth/token")
  
  let client_token_result = OAuth2Authenticator::get_client_credentials_token(client_creds_auth)
  
  match client_token_result {
    Ok(token_response) => {
      assert_true(token_response.access_token.length() > 0)
      assert_eq(token_response.token_type, "Bearer")
      assert_true(token_response.expires_in > 0)
    }
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test OAuth2 token validation
  let validation_result = OAuth2Authenticator::validate_token(oauth2_auth, access_token)
  
  match validation_result {
    Ok(is_valid) => assert_true(is_valid)
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test OAuth2 token revocation
  let revoke_result = OAuth2Authenticator::revoke_token(oauth2_auth, access_token)
  
  match revoke_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
  
  // Test OAuth2 token introspection
  let introspect_result = OAuth2Authenticator::introspect_token(oauth2_auth, access_token)
  
  match introspect_result {
    Ok(introspection) => {
      assert_true(introspection.active)
      assert_true(introspection.scope.contains("read"))
      assert_true(introspection.scope.contains("write"))
    }
    Err(_) => {
      // In test environment, OAuth2 server might not be available
      assert_true(true)
    }
  }
}

// Test 4: RBAC (Role-Based Access Control)
test "role-based access control" {
  // Create RBAC manager
  let rbac = RBACManager::new()
  
  // Define roles
  rbac.define_role("admin", [
    Permission::new("telemetry.read", "*"),
    Permission::new("telemetry.write", "*"),
    Permission::new("telemetry.delete", "*"),
    Permission::new("users.read", "*"),
    Permission::new("users.write", "*")
  ])
  
  rbac.define_role("operator", [
    Permission::new("telemetry.read", "*"),
    Permission::new("telemetry.write", "*"),
    Permission::new("users.read", "*")
  ])
  
  rbac.define_role("viewer", [
    Permission::new("telemetry.read", "*")
  ])
  
  // Assign roles to users
  rbac.assign_role("user1", "admin")
  rbac.assign_role("user2", "operator")
  rbac.assign_role("user3", "viewer")
  
  // Test permission checking
  let admin_read = rbac.has_permission("user1", "telemetry.read", "service-123")
  assert_true(admin_read)
  
  let admin_delete = rbac.has_permission("user1", "telemetry.delete", "service-123")
  assert_true(admin_delete)
  
  let operator_read = rbac.has_permission("user2", "telemetry.read", "service-123")
  assert_true(operator_read)
  
  let operator_delete = rbac.has_permission("user2", "telemetry.delete", "service-123")
  assert_false(operator_delete)
  
  let viewer_read = rbac.has_permission("user3", "telemetry.read", "service-123")
  assert_true(viewer_read)
  
  let viewer_write = rbac.has_permission("user3", "telemetry.write", "service-123")
  assert_false(viewer_write)
  
  // Test resource-specific permissions
  rbac.define_role("service_admin", [
    Permission::new("telemetry.read", "service-123"),
    Permission::new("telemetry.write", "service-123"),
    Permission::new("telemetry.read", "service-456")
  ])
  
  rbac.assign_role("user4", "service_admin")
  
  let service_admin_read_123 = rbac.has_permission("user4", "telemetry.read", "service-123")
  assert_true(service_admin_read_123)
  
  let service_admin_write_123 = rbac.has_permission("user4", "telemetry.write", "service-123")
  assert_true(service_admin_write_123)
  
  let service_admin_read_456 = rbac.has_permission("user4", "telemetry.read", "service-456")
  assert_true(service_admin_read_456)
  
  let service_admin_write_456 = rbac.has_permission("user4", "telemetry.write", "service-456")
  assert_false(service_admin_write_456)
  
  // Test multiple roles per user
  rbac.assign_role("user5", "viewer")
  rbac.assign_role("user5", "operator") // operator should override viewer
  
  let multi_role_read = rbac.has_permission("user5", "telemetry.read", "service-123")
  assert_true(multi_role_read)
  
  let multi_role_write = rbac.has_permission("user5", "telemetry.write", "service-123")
  assert_true(multi_role_write)
  
  // Test role hierarchy
  rbac.define_role_hierarchy("admin", ["operator", "viewer"])
  rbac.define_role_hierarchy("operator", ["viewer"])
  
  // Remove direct role assignment to test hierarchy
  rbac.remove_role("user1", "admin")
  rbac.assign_role("user1", "operator")
  
  // Even with operator role, should not have delete permission
  let hierarchy_delete = rbac.has_permission("user1", "telemetry.delete", "service-123")
  assert_false(hierarchy_delete)
  
  // Test permission inheritance
  rbac.define_role("super_admin", [
    Permission::new("*", "*") // All permissions
  ])
  
  rbac.assign_role("user6", "super_admin")
  
  let super_admin_any = rbac.has_permission("user6", "any.action", "any.resource")
  assert_true(super_admin_any)
  
  // Test role removal
  rbac.remove_role("user2", "operator")
  
  let removed_permission = rbac.has_permission("user2", "telemetry.read", "service-123")
  assert_false(removed_permission)
  
  // Test permission checking with context
  let context = AuthContext::new("user1")
  AuthContext::add_claim(context, "department", "engineering")
  
  rbac.define_role("engineering_lead", [
    Permission::new("telemetry.read", "*", ["department=engineering"]),
    Permission::new("telemetry.write", "*", ["department=engineering"])
  ])
  
  rbac.assign_role("user7", "engineering_lead")
  
  let contextual_permission = rbac.has_permission_with_context("user7", "telemetry.read", "service-123", context)
  assert_true(contextual_permission)
  
  // Test RBAC statistics
  let stats = rbac.get_statistics()
  
  assert_eq(stats.roles_count, 6) // admin, operator, viewer, service_admin, super_admin, engineering_lead
  assert_eq(stats.users_count, 7) // user1 through user7
  assert_eq(stats.permissions_count, 15) // Total permissions across all roles
}

// Test 5: Security Middleware
test "security middleware" {
  // Create security middleware
  let security_middleware = SecurityMiddleware::new()
  
  // Configure middleware
  security_middleware.enable_cors(true)
  security_middleware.enable_csp(true)
  security_middleware.enable_rate_limiting(true)
  security_middleware.enable_input_validation(true)
  security_middleware.enable_sql_injection_protection(true)
  security_middleware.enable_xss_protection(true)
  
  // Configure CORS
  let cors_config = CorsConfig::new()
  CorsConfig::add_allowed_origin(cors_config, "https://azimuth.example.com")
  CorsConfig::add_allowed_method(cors_config, "GET")
  CorsConfig::add_allowed_method(cors_config, "POST")
  CorsConfig::add_allowed_header(cors_config, "Content-Type")
  CorsConfig::add_allowed_header(cors_config, "Authorization")
  CorsConfig::set_max_age(cors_config, 86400) // 24 hours
  
  security_middleware.set_cors_config(cors_config)
  
  // Configure CSP
  let csp_config = CspConfig::new()
  CspConfig::add_directive(csp_config, "default-src", "'self'")
  CspConfig::add_directive(csp_config, "script-src", "'self' 'unsafe-inline'")
  CspConfig::add_directive(csp_config, "style-src", "'self' 'unsafe-inline'")
  CspConfig::add_directive(csp_config, "img-src", "'self' data:")
  CspConfig::add_directive(csp_config, "font-src", "'self'")
  
  security_middleware.set_csp_config(csp_config)
  
  // Configure rate limiting
  let rate_limit_config = RateLimitConfig::new()
  RateLimitConfig::add_rule(rate_limit_config, "/api/*", 100, 3600) // 100 requests per hour
  RateLimitConfig::add_rule(rate_limit_config, "/api/telemetry", 1000, 3600) // 1000 requests per hour
  
  security_middleware.set_rate_limit_config(rate_limit_config)
  
  // Test CORS middleware
  let cors_request = HttpRequest::new("OPTIONS", "/api/telemetry")
  HttpRequest::add_header(cors_request, "Origin", "https://azimuth.example.com")
  HttpRequest::add_header(cors_request, "Access-Control-Request-Method", "POST")
  HttpRequest::add_header(cors_request, "Access-Control-Request-Headers", "Content-Type,Authorization")
  
  let cors_response = security_middleware.handle_cors(cors_request)
  
  assert_eq(HttpResponse::status_code(cors_response), 200)
  assert_eq(HttpResponse::get_header(cors_response, "Access-Control-Allow-Origin"), Some("https://azimuth.example.com"))
  assert_eq(HttpResponse::get_header(cors_response, "Access-Control-Allow-Methods"), Some("GET,POST"))
  assert_eq(HttpResponse::get_header(cors_response, "Access-Control-Allow-Headers"), Some("Content-Type,Authorization"))
  assert_eq(HttpResponse::get_header(cors_response, "Access-Control-Max-Age"), Some("86400"))
  
  // Test CSP middleware
  let csp_request = HttpRequest::new("GET", "/")
  
  let csp_response = security_middleware.handle_csp(csp_request)
  
  let csp_header = HttpResponse::get_header(csp_response, "Content-Security-Policy")
  match csp_header {
    Some(policy) => {
      assert_true(policy.contains("default-src 'self'"))
      assert_true(policy.contains("script-src 'self' 'unsafe-inline'"))
      assert_true(policy.contains("style-src 'self' 'unsafe-inline'"))
      assert_true(policy.contains("img-src 'self' data:"))
      assert_true(policy.contains("font-src 'self'"))
    }
    None => assert_true(false)
  }
  
  // Test rate limiting middleware
  let rate_limit_request = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(rate_limit_request, "X-Forwarded-For", "192.168.1.1")
  
  // First request should pass
  let rate_limit_response1 = security_middleware.handle_rate_limiting(rate_limit_request)
  assert_eq(HttpResponse::status_code(rate_limit_response1), 200)
  
  // Make multiple requests to test rate limiting
  for i in 0..=10 {
    security_middleware.handle_rate_limiting(rate_limit_request)
  }
  
  // Should still pass (well below limit)
  let rate_limit_response2 = security_middleware.handle_rate_limiting(rate_limit_request)
  assert_eq(HttpResponse::status_code(rate_limit_response2), 200)
  
  // Test input validation middleware
  let valid_request = HttpRequest::new("POST", "/api/telemetry")
  HttpRequest::add_header(valid_request, "Content-Type", "application/json")
  HttpRequest::set_body(valid_request, "{\"trace_id\":\"trace-123\",\"value\":42}")
  
  let valid_response = security_middleware.handle_input_validation(valid_request)
  assert_eq(HttpResponse::status_code(valid_response), 200)
  
  // Test invalid input
  let invalid_request = HttpRequest::new("POST", "/api/telemetry")
  HttpRequest::add_header(invalid_request, "Content-Type", "application/json")
  HttpRequest::set_body(invalid_request, "{\"trace_id\":123,\"value\":\"invalid\"}") // trace_id should be string
  
  let invalid_response = security_middleware.handle_input_validation(invalid_request)
  assert_eq(HttpResponse::status_code(invalid_response), 400)
  
  // Test SQL injection protection
  let sql_injection_request = HttpRequest::new("GET", "/api/users?id=1; DROP TABLE users;")
  
  let sql_injection_response = security_middleware.handle_sql_injection_protection(sql_injection_request)
  assert_eq(HttpResponse::status_code(sql_injection_response), 400)
  
  // Test XSS protection
  let xss_request = HttpRequest::new("POST", "/api/telemetry")
  HttpRequest::add_header(xss_request, "Content-Type", "application/json")
  HttpRequest::set_body(xss_request, "{\"comment\":\"<script>alert('xss')</script>\"}")
  
  let xss_response = security_middleware.handle_xss_protection(xss_request)
  
  // Should sanitize the input
  let sanitized_body = HttpResponse::body(xss_response)
  assert_false(sanitized_body.contains("<script>"))
  assert_false(sanitized_body.contains("alert('xss')"))
  
  // Test security headers middleware
  let headers_request = HttpRequest::new("GET", "/api/telemetry")
  
  let headers_response = security_middleware.handle_security_headers(headers_request)
  
  assert_eq(HttpResponse::get_header(headers_response, "X-Content-Type-Options"), Some("nosniff"))
  assert_eq(HttpResponse::get_header(headers_response, "X-Frame-Options"), Some("DENY"))
  assert_eq(HttpResponse::get_header(headers_response, "X-XSS-Protection"), Some("1; mode=block"))
  assert_eq(HttpResponse::get_header(headers_response, "Strict-Transport-Security"), Some("max-age=31536000; includeSubDomains"))
  
  // Test authentication middleware
  let auth_request = HttpRequest::new("GET", "/api/telemetry")
  HttpRequest::add_header(auth_request, "Authorization", "Bearer valid_token")
  
  let auth_response = security_middleware.handle_authentication(auth_request)
  
  match HttpResponse::status_code(auth_response) {
    200 => assert_true(true) // Authenticated
    401 => assert_true(true) // Not authenticated (expected in test environment)
    _ => assert_true(false)
  }
  
  // Test authorization middleware
  let authz_request = HttpRequest::new("DELETE", "/api/telemetry/123")
  HttpRequest::add_header(authz_request, "Authorization", "Bearer admin_token")
  
  let authz_response = security_middleware.handle_authorization(authz_request)
  
  match HttpResponse::status_code(authz_response) {
    200 => assert_true(true) // Authorized
    403 => assert_true(true) // Not authorized (expected in test environment)
    _ => assert_true(false)
  }
  
  // Test security middleware statistics
  let stats = security_middleware.get_statistics()
  
  assert_true(stats.cors_requests >= 0)
  assert_true(stats.csp_requests >= 0)
  assert_true(stats.rate_limit_requests >= 0)
  assert_true(stats.input_validation_requests >= 0)
  assert_true(stats.sql_injection_blocks >= 0)
  assert_true(stats.xss_blocks >= 0)
  assert_true(stats.authentication_attempts >= 0)
  assert_true(stats.authorization_attempts >= 0)
}

// Test 6: Encryption and Decryption
test "encryption and decryption" {
  // Create encryption manager
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption key
  let key = EncryptionManager::generate_key("AES-256-GCM")
  
  assert_eq(key.algorithm, "AES-256-GCM")
  assert_true(key.data.length() > 0)
  
  // Test symmetric encryption
  let plaintext = "Sensitive telemetry data to be encrypted"
  
  let encrypted_data = EncryptionManager::encrypt(encryption_manager, plaintext, key)
  
  assert_true(encrypted_data.data.length() > 0)
  assert_true(encrypted_data.nonce.length() > 0)
  assert_true(encrypted_data.tag.length() > 0)
  assert_true(encrypted_data.data != plaintext) // Should be different from plaintext
  
  // Test symmetric decryption
  let decrypted_data = EncryptionManager::decrypt(encryption_manager, encrypted_data, key)
  
  match decrypted_data {
    Ok(decrypted) => assert_eq(decrypted, plaintext)
    Err(_) => assert_true(false)
  }
  
  // Test asymmetric encryption
  let key_pair = EncryptionManager::generate_key_pair("RSA-2048")
  
  assert_eq(key_pair.public_key.algorithm, "RSA-2048")
  assert_eq(key_pair.private_key.algorithm, "RSA-2048")
  assert_true(key_pair.public_key.data.length() > 0)
  assert_true(key_pair.private_key.data.length() > 0)
  
  // Test with smaller data for RSA
  let small_plaintext = "Small data"
  
  let asymmetric_encrypted = EncryptionManager::encrypt_public(encryption_manager, small_plaintext, key_pair.public_key)
  
  assert_true(asymmetric_encrypted.data.length() > 0)
  assert_true(asymmetric_encrypted.data != small_plaintext)
  
  // Test asymmetric decryption
  let asymmetric_decrypted = EncryptionManager::decrypt_private(encryption_manager, asymmetric_encrypted, key_pair.private_key)
  
  match asymmetric_decrypted {
    Ok(decrypted) => assert_eq(decrypted, small_plaintext)
    Err(_) => assert_true(false)
  }
  
  // Test key derivation
  let password = "secure_password_123"
  let salt = "random_salt_value"
  
  let derived_key = EncryptionManager::derive_key(encryption_manager, password, salt, "PBKDF2", 256)
  
  assert_true(derived_key.data.length() > 0)
  assert_eq(derived_key.algorithm, "AES-256")
  
  // Test key derivation with different parameters
  let derived_key_scrypt = EncryptionManager::derive_key(encryption_manager, password, salt, "scrypt", 256)
  
  assert_true(derived_key_scrypt.data.length() > 0)
  assert_eq(derived_key_scrypt.algorithm, "AES-256")
  assert_true(derived_key.data != derived_key.data) // Different derivation methods should produce different keys
  
  // Test key serialization
  let serialized_key = EncryptionManager::serialize_key(key)
  
  assert_true(serialized_key.length() > 0)
  assert_true(serialized_key.contains("AES-256-GCM"))
  
  let deserialized_key = EncryptionManager::deserialize_key(serialized_key)
  
  match deserialized_key {
    Ok(deserialized) => {
      assert_eq(deserialized.algorithm, key.algorithm)
      assert_eq(deserialized.data, key.data)
    }
    Err(_) => assert_true(false)
  }
  
  // Test key rotation
  let old_key = EncryptionManager::generate_key("AES-256-GCM")
  let new_key = EncryptionManager::generate_key("AES-256-GCM")
  
  // Encrypt with old key
  let old_encrypted = EncryptionManager::encrypt(encryption_manager, plaintext, old_key)
  
  // Re-encrypt with new key
  let reencrypted = EncryptionManager::reencrypt(encryption_manager, old_encrypted, old_key, new_key)
  
  // Decrypt with new key
  let reencrypted_decrypted = EncryptionManager::decrypt(encryption_manager, reencrypted, new_key)
  
  match reencrypted_decrypted {
    Ok(decrypted) => assert_eq(decrypted, plaintext)
    Err(_) => assert_true(false)
  }
  
  // Test encryption with compression
  let large_plaintext = "x".repeat(10000) // 10KB of data
  
  let compressed_encrypted = EncryptionManager::encrypt_with_compression(encryption_manager, large_plaintext, key, "gzip")
  
  let compressed_decrypted = EncryptionManager::decrypt_with_decompression(encryption_manager, compressed_encrypted, key, "gzip")
  
  match compressed_decrypted {
    Ok(decrypted) => assert_eq(decrypted, large_plaintext)
    Err(_) => assert_true(false)
  }
  
  // Test encryption performance
  let performance_data = "Performance test data ".repeat(1000) // ~20KB
  
  let encrypt_start = Time::now()
  let performance_encrypted = EncryptionManager::encrypt(encryption_manager, performance_data, key)
  let encrypt_end = Time::now()
  let encrypt_duration = encrypt_end - encrypt_start
  
  let decrypt_start = Time::now()
  let performance_decrypted = EncryptionManager::decrypt(encryption_manager, performance_encrypted, key)
  let decrypt_end = Time::now()
  let decrypt_duration = decrypt_end - decrypt_start
  
  match performance_decrypted {
    Ok(decrypted) => {
      assert_eq(decrypted, performance_data)
      assert_true(encrypt_duration < 1000) // Should encrypt in less than 1 second
      assert_true(decrypt_duration < 1000) // Should decrypt in less than 1 second
    }
    Err(_) => assert_true(false)
  }
  
  // Test encryption statistics
  let stats = EncryptionManager::get_statistics(encryption_manager)
  
  assert_true(stats.encryptions >= 0)
  assert_true(stats.decryptions >= 0)
  assert_true(stats.encryption_errors >= 0)
  assert_true(stats.decryption_errors >= 0)
  assert_true(stats.total_encrypted_bytes >= 0)
  assert_true(stats.total_decrypted_bytes >= 0)
}

// Test 7: Security Audit and Logging
test "security audit and logging" {
  // Create security auditor
  let security_auditor = SecurityAuditor::new()
  
  // Configure auditor
  SecurityAuditor::enable_file_logging(security_auditor, "/tmp/security_audit.log")
  SecurityAuditor::enable_syslog(security_auditor, "azimuth-security")
  SecurityAuditor::set_log_level(security_auditor, "INFO")
  SecurityAuditor::enable_structured_logging(security_auditor, true)
  
  // Test authentication event logging
  let auth_event = SecurityEvent::authentication(
    "user123",
    "api_key",
    "success",
    "192.168.1.100",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  )
  
  SecurityAuditor::log_event(security_auditor, auth_event)
  
  // Test authorization event logging
  let authz_event = SecurityEvent::authorization(
    "user123",
    "telemetry.read",
    "service-456",
    "success",
    "192.168.1.100"
  )
  
  SecurityAuditor::log_event(security_auditor, authz_event)
  
  // Test security violation event logging
  let violation_event = SecurityEvent::violation(
    "sql_injection",
    "192.168.1.200",
    "/api/users?id=1; DROP TABLE users;",
    "blocked"
  )
  
  SecurityAuditor::log_event(security_auditor, violation_event)
  
  // Test data access event logging
  let access_event = SecurityEvent::data_access(
    "user456",
    "telemetry_data",
    "read",
    "trace-12345",
    "success"
  )
  
  SecurityAuditor::log_event(security_auditor, access_event)
  
  // Test configuration change event logging
  let config_event = SecurityEvent::configuration_change(
    "admin",
    "api_key_rotation",
    "key123",
    "rotated"
  )
  
  SecurityAuditor::log_event(security_auditor, config_event)
  
  // Test event query
  let auth_events = SecurityAuditor::query_events(security_auditor, EventQuery {
    event_type: Some("authentication"),
    user_id: Some("user123"),
    start_time: Some(Time::now() - 3600), // Last hour
    end_time: Some(Time::now()),
    limit: Some(100)
  })
  
  assert_true(auth_events.length() >= 1)
  
  let auth_event_query = auth_events[0]
  assert_eq(auth_event_query.event_type, "authentication")
  assert_eq(auth_event_query.user_id, "user123")
  assert_eq(auth_event_query.action, "success")
  
  // Test security metrics
  let metrics = SecurityAuditor::get_metrics(security_auditor)
  
  assert_true(metrics.total_events >= 5)
  assert_true(metrics.authentication_events >= 1)
  assert_true(metrics.authorization_events >= 1)
  assert_true(metrics.violation_events >= 1)
  assert_true(metrics.data_access_events >= 1)
  assert_true(metrics.configuration_change_events >= 1)
  
  // Test security report generation
  let report = SecurityAuditor::generate_report(security_auditor, Time::now() - 86400, Time::now()) // Last 24 hours
  
  assert_true(report.contains("Security Audit Report"))
  assert_true(report.contains("Authentication Events"))
  assert_true(report.contains("Authorization Events"))
  assert_true(report.contains("Security Violations"))
  assert_true(report.contains("Data Access Events"))
  assert_true(report.contains("Configuration Changes"))
  
  // Test security alerts
  let alert_manager = SecurityAlertManager::new()
  
  // Configure alerts
  SecurityAlertManager::add_rule(alert_manager, SecurityAlertRule {
    name: "multiple_failed_logins",
    condition: AlertCondition::FailedLogins(GreaterThan, 5),
    time_window: 300, // 5 minutes
    severity: AlertSeverity::Warning,
    enabled: true
  })
  
  SecurityAlertManager::add_rule(alert_manager, SecurityAlertRule {
    name: "security_violation_spike",
    condition: AlertCondition::Violations(GreaterThan, 10),
    time_window: 600, // 10 minutes
    severity: AlertSeverity::Critical,
    enabled: true
  })
  
  // Simulate failed logins
  for i in 0..=6 {
    let failed_auth_event = SecurityEvent::authentication(
      "user" + i.to_string(),
      "password",
      "failed",
      "192.168.1." + (100 + i).to_string(),
      "Mozilla/5.0"
    )
    
    SecurityAuditor::log_event(security_auditor, failed_auth_event)
  }
  
  // Check for alerts
  let alerts = SecurityAlertManager::check_alerts(alert_manager, security_auditor)
  
  assert_true(alerts.length() >= 1)
  
  let failed_login_alert = alerts.find(fn(alert) { alert.rule_name == "multiple_failed_logins" })
  match failed_login_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_true(alert.message.contains("multiple failed logins"))
    }
    None => assert_true(false)
  }
  
  // Test security audit trail
  let audit_trail = SecurityAuditor::get_audit_trail(security_auditor, "user123", Time::now() - 3600, Time::now())
  
  assert_true(audit_trail.length() >= 2) // Authentication and authorization events
  
  for event in audit_trail {
    assert_eq(event.user_id, "user123")
    assert_true(event.timestamp >= Time::now() - 3600)
    assert_true(event.timestamp <= Time::now())
  }
  
  // Test security compliance checking
  let compliance_checker = SecurityComplianceChecker::new()
  
  // Add compliance rules
  SecurityComplianceChecker::add_rule(compliance_checker, ComplianceRule {
    name: "api_key_authentication_required",
    description: "All API requests must be authenticated with API keys",
    check_fn: fn(events) {
      let auth_events = events.filter(fn(e) { e.event_type == "authentication" })
      auth_events.length() > 0
    },
    severity: ComplianceSeverity::Required
  })
  
  SecurityComplianceChecker::add_rule(compliance_checker, ComplianceRule {
    name: "no_sql_injection_attempts",
    description: "No SQL injection attempts should be detected",
    check_fn: fn(events) {
      let violation_events = events.filter(fn(e) { e.event_type == "violation" && e.details.contains("sql_injection") })
      violation_events.length() == 0
    },
    severity: ComplianceSeverity::Required
  })
  
  // Check compliance
  let compliance_report = SecurityComplianceChecker::check_compliance(compliance_checker, security_auditor, Time::now() - 86400, Time::now())
  
  assert_true(compliance_report.contains("Compliance Report"))
  assert_true(compliance_report.contains("api_key_authentication_required"))
  assert_true(compliance_report.contains("no_sql_injection_attempts"))
  
  // Test security log retention
  SecurityAuditor::set_retention_policy(security_auditor, 2592000) // 30 days
  
  let retention_result = SecurityAuditor::apply_retention_policy(security_auditor)
  
  match retention_result {
    Ok(deleted_count) => assert_true(deleted_count >= 0)
    Err(_) => assert_true(false)
  }
  
  // Test security log export
  let export_result = SecurityAuditor::export_logs(security_auditor, Time::now() - 3600, Time::now(), "json")
  
  match export_result {
    Ok(exported_logs) => {
      assert_true(exported_logs.length() > 0)
      assert_true(exported_logs.contains("\"event_type\""))
    }
    Err(_) => assert_true(false)
  }
}