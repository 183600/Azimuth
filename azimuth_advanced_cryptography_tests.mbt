// Azimuth Telemetry System - Advanced Cryptography Tests
// This file contains test cases for advanced cryptographic operations used in the telemetry system

// Test 1: Hash Functions
test "hash functions" {
  let input_data = "Hello, World!"
  
  // Test SHA-256 hashing
  let sha256_hash = CryptoUtil::sha256_hash(input_data)
  assert_eq(sha256_hash.length(), 64) // SHA-256 produces 64 hex characters
  assert_eq(sha256_hash, "dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f")
  
  // Test SHA-512 hashing
  let sha512_hash = CryptoUtil::sha512_hash(input_data)
  assert_eq(sha512_hash.length(), 128) // SHA-512 produces 128 hex characters
  assert_eq(sha512_hash, "87428fc522803d31065e7bce3cf03fe475096631e5e07bbd7a0fde60c4cf25c51d42fd42774b6652d3d198885e5c169721488a22b520b4d96cf28eb42f883c89")
  
  // Test MD5 hashing
  let md5_hash = CryptoUtil::md5_hash(input_data)
  assert_eq(md5_hash.length(), 32) // MD5 produces 32 hex characters
  assert_eq(md5_hash, "ed076287532e86365e841e92bfc50d8c")
  
  // Test hash verification
  assert_true(CryptoUtil::verify_sha256(input_data, sha256_hash))
  assert_false(CryptoUtil::verify_sha256("Different input", sha256_hash))
  
  assert_true(CryptoUtil::verify_sha512(input_data, sha512_hash))
  assert_false(CryptoUtil::verify_sha512("Different input", sha512_hash))
  
  assert_true(CryptoUtil::verify_md5(input_data, md5_hash))
  assert_false(CryptoUtil::verify_md5("Different input", md5_hash))
}

// Test 2: HMAC Operations
test "hmac operations" {
  let message = "This is a test message"
  let secret_key = "secret_key_12345"
  
  // Test HMAC-SHA256
  let hmac_sha256 = CryptoUtil::hmac_sha256(message, secret_key)
  assert_eq(hmac_sha256.length(), 64)
  assert_eq(hmac_sha256, "f6b3b8b1b6e1c7d3a9f5e2b4c8d0f1a2e3c4b5d6f7a8b9c0d1e2f3a4b5c6d7e8")
  
  // Test HMAC-SHA512
  let hmac_sha512 = CryptoUtil::hmac_sha512(message, secret_key)
  assert_eq(hmac_sha512.length(), 128)
  assert_eq(hmac_sha512, "8a2b4c6d8e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1")
  
  // Test HMAC verification
  assert_true(CryptoUtil::verify_hmac_sha256(message, secret_key, hmac_sha256))
  assert_false(CryptoUtil::verify_hmac_sha256("Different message", secret_key, hmac_sha256))
  assert_false(CryptoUtil::verify_hmac_sha256(message, "different_key", hmac_sha256))
  
  assert_true(CryptoUtil::verify_hmac_sha512(message, secret_key, hmac_sha512))
  assert_false(CryptoUtil::verify_hmac_sha512("Different message", secret_key, hmac_sha512))
  assert_false(CryptoUtil::verify_hmac_sha512(message, "different_key", hmac_sha512))
}

// Test 3: Symmetric Encryption
test "symmetric encryption" {
  let plaintext = "This is a secret message"
  let password = "secure_password_123"
  
  // Test AES-256 encryption
  let encrypted_data = CryptoUtil::aes256_encrypt(plaintext, password)
  assert_true(encrypted_data.length() > 0)
  assert_ne(encrypted_data, plaintext) // Should be different from plaintext
  
  // Test AES-256 decryption
  let decrypted_data = CryptoUtil::aes256_decrypt(encrypted_data, password)
  match decrypted_data {
    Some(value) => assert_eq(value, plaintext)
    None => assert_true(false)
  }
  
  // Test decryption with wrong password
  let wrong_password_decrypted = CryptoUtil::aes256_decrypt(encrypted_data, "wrong_password")
  match wrong_password_decrypted {
    Some(_) => assert_true(false) // Should not decrypt with wrong password
    None => assert_true(true)
  }
  
  // Test encryption with empty plaintext
  let empty_encrypted = CryptoUtil::aes256_encrypt("", password)
  assert_true(empty_encrypted.length() > 0)
  
  let empty_decrypted = CryptoUtil::aes256_decrypt(empty_encrypted, password)
  match empty_decrypted {
    Some(value) => assert_eq(value, "")
    None => assert_true(false)
  }
}

// Test 4: Asymmetric Encryption
test "asymmetric encryption" {
  // Generate RSA key pair
  let key_pair = CryptoUtil::generate_rsa_keypair(2048)
  let public_key = CryptoUtil::get_public_key(key_pair)
  let private_key = CryptoUtil::get_private_key(key_pair)
  
  // Test RSA encryption with public key
  let message = "This message will be encrypted with RSA"
  let encrypted_message = CryptoUtil::rsa_encrypt(message, public_key)
  assert_true(encrypted_message.length() > 0)
  assert_ne(encrypted_message, message)
  
  // Test RSA decryption with private key
  let decrypted_message = CryptoUtil::rsa_decrypt(encrypted_message, private_key)
  match decrypted_message {
    Some(value) => assert_eq(value, message)
    None => assert_true(false)
  }
  
  // Test RSA decryption with wrong key
  let wrong_key_pair = CryptoUtil::generate_rsa_keypair(2048)
  let wrong_private_key = CryptoUtil::get_private_key(wrong_key_pair)
  let wrong_decrypted = CryptoUtil::rsa_decrypt(encrypted_message, wrong_private_key)
  match wrong_decrypted {
    Some(_) => assert_true(false) // Should not decrypt with wrong key
    None => assert_true(true)
  }
  
  // Test digital signature
  let document = "Important document to sign"
  let signature = CryptoUtil::rsa_sign(document, private_key)
  assert_true(signature.length() > 0)
  
  // Test signature verification
  assert_true(CryptoUtil::rsa_verify(document, signature, public_key))
  assert_false(CryptoUtil::rsa_verify("Modified document", signature, public_key))
  
  // Test key serialization
  let serialized_public = CryptoUtil::serialize_public_key(public_key)
  let serialized_private = CryptoUtil::serialize_private_key(private_key)
  
  assert_true(serialized_public.length() > 0)
  assert_true(serialized_private.length() > 0)
  
  // Test key deserialization
  let deserialized_public = CryptoUtil::deserialize_public_key(serialized_public)
  let deserialized_private = CryptoUtil::deserialize_private_key(serialized_private)
  
  // Test with deserialized keys
  let re_encrypted = CryptoUtil::rsa_encrypt(message, deserialized_public)
  let re_decrypted = CryptoUtil::rsa_decrypt(re_encrypted, deserialized_private)
  match re_decrypted {
    Some(value) => assert_eq(value, message)
    None => assert_true(false)
  }
}

// Test 5: Key Derivation
test "key derivation" {
  let password = "user_password_123"
  let salt = "random_salt_value"
  
  // Test PBKDF2 key derivation
  let derived_key = CryptoUtil::pbkdf2_derive(password, salt, 10000, 32) // 32 bytes key
  assert_eq(derived_key.length(), 64) // 32 bytes = 64 hex characters
  
  // Test deterministic derivation
  let derived_key2 = CryptoUtil::pbkdf2_derive(password, salt, 10000, 32)
  assert_eq(derived_key, derived_key2) // Same input should produce same output
  
  // Test different parameters produce different keys
  let different_iterations = CryptoUtil::pbkdf2_derive(password, salt, 20000, 32)
  assert_ne(derived_key, different_iterations)
  
  let different_salt = CryptoUtil::pbkdf2_derive(password, "different_salt", 10000, 32)
  assert_ne(derived_key, different_salt)
  
  let different_password = CryptoUtil::pbkdf2_derive("different_password", salt, 10000, 32)
  assert_ne(derived_key, different_password)
  
  // Test key verification
  assert_true(CryptoUtil::verify_pbkdf2(password, salt, 10000, 32, derived_key))
  assert_false(CryptoUtil::verify_pbkdf2("wrong_password", salt, 10000, 32, derived_key))
}

// Test 6: Random Number Generation
test "random number generation" {
  // Test secure random bytes
  let random_bytes1 = CryptoUtil::secure_random_bytes(16)
  let random_bytes2 = CryptoUtil::secure_random_bytes(16)
  
  assert_eq(random_bytes1.length(), 32) // 16 bytes = 32 hex characters
  assert_eq(random_bytes2.length(), 32)
  assert_ne(random_bytes1, random_bytes2) // Should be different
  
  // Test secure random string
  let random_string1 = CryptoUtil::secure_random_string(10)
  let random_string2 = CryptoUtil::secure_random_string(10)
  
  assert_eq(random_string1.length(), 10)
  assert_eq(random_string2.length(), 10)
  assert_ne(random_string1, random_string2) // Should be different
  
  // Test secure random integer
  let random_int1 = CryptoUtil::secure_random_int(1, 100)
  let random_int2 = CryptoUtil::secure_random_int(1, 100)
  
  assert_true(random_int1 >= 1)
  assert_true(random_int1 <= 100)
  assert_true(random_int2 >= 1)
  assert_true(random_int2 <= 100)
  
  // Test UUID generation
  let uuid1 = CryptoUtil::generate_uuid()
  let uuid2 = CryptoUtil::generate_uuid()
  
  assert_eq(uuid1.length(), 36) // Standard UUID format: 8-4-4-4-12
  assert_eq(uuid2.length(), 36)
  assert_ne(uuid1, uuid2) // Should be different
  
  // Test UUID format validation
  assert_true(CryptoUtil::is_valid_uuid(uuid1))
  assert_false(CryptoUtil::is_valid_uuid("not-a-uuid"))
  assert_false(CryptoUtil::is_valid_uuid("123e4567-e89b-12d3-a456-42661417400")) // Too short
}

// Test 7: Password Hashing
test "password hashing" {
  let password = "user_secure_password"
  
  // Test bcrypt hashing
  let bcrypt_hash = CryptoUtil::bcrypt_hash(password, 12) // Cost factor of 12
  assert_true(bcrypt_hash.length() > 50) // Bcrypt hashes are typically 60 characters
  assert_ne(bcrypt_hash, password)
  
  // Test bcrypt verification
  assert_true(CryptoUtil::bcrypt_verify(password, bcrypt_hash))
  assert_false(CryptoUtil::bcrypt_verify("wrong_password", bcrypt_hash))
  
  // Test argon2 hashing
  let argon2_hash = CryptoUtil::argon2_hash(password)
  assert_true(argon2_hash.length() > 50)
  assert_ne(argon2_hash, password)
  
  // Test argon2 verification
  assert_true(CryptoUtil::argon2_verify(password, argon2_hash))
  assert_false(CryptoUtil::argon2_verify("wrong_password", argon2_hash))
  
  // Test password strength validation
  let weak_password = "123"
  let strong_password = "Str0ngP@ssw0rd!"
  
  assert_false(CryptoUtil::is_strong_password(weak_password))
  assert_true(CryptoUtil::is_strong_password(strong_password))
  
  // Test password generation
  let generated_password = CryptoUtil::generate_strong_password(16)
  assert_eq(generated_password.length(), 16)
  assert_true(CryptoUtil::is_strong_password(generated_password))
}