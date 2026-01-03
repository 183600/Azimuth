// Azimuth Telemetry System - Enhanced Data Validation Tests
// This file contains comprehensive test cases for enhanced data validation functionality

// Test 1: String Data Validation
test "string data validation" {
  // Test valid string validation
  let valid_string = "valid_test_string"
  assert_true(DataValidator::is_valid_string(valid_string))
  
  // Test empty string validation
  let empty_string = ""
  assert_false(DataValidator::is_valid_string(empty_string))
  
  // Test string with only spaces
  let spaces_string = "   "
  assert_false(DataValidator::is_valid_string(spaces_string))
  
  // Test string with special characters
  let special_chars_string = "test@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  assert_true(DataValidator::is_valid_string(special_chars_string))
  
  // Test string length validation
  let short_string = "abc"
  assert_true(DataValidator::is_valid_string_length(short_string, 1, 10))
  
  let long_string = "this_is_a_very_long_string_that_exceeds_the_maximum_allowed_length"
  assert_false(DataValidator::is_valid_string_length(long_string, 1, 50))
}

// Test 2: Numeric Data Validation
test "numeric data validation" {
  // Test valid integer validation
  let valid_int = 42
  assert_true(DataValidator::is_valid_integer(valid_int))
  
  // Test zero validation
  let zero = 0
  assert_true(DataValidator::is_valid_integer(zero))
  
  // Test negative integer validation
  let negative_int = -10
  assert_true(DataValidator::is_valid_integer(negative_int))
  
  // Test integer range validation
  let in_range_int = 50
  assert_true(DataValidator::is_valid_integer_range(in_range_int, 1, 100))
  
  let out_of_range_int = 150
  assert_false(DataValidator::is_valid_integer_range(out_of_range_int, 1, 100))
  
  // Test valid float validation
  let valid_float = 3.14
  assert_true(DataValidator::is_valid_float(valid_float))
  
  // Test float precision validation
  let precise_float = 3.14159265359
  assert_true(DataValidator::is_valid_float_precision(precise_float, 10))
  
  let imprecise_float = 3.14159265359
  assert_false(DataValidator::is_valid_float_precision(imprecise_float, 5))
}

// Test 3: Boolean Data Validation
test "boolean data validation" {
  // Test valid boolean values
  let true_value = true
  assert_true(DataValidator::is_valid_boolean(true_value))
  
  let false_value = false
  assert_true(DataValidator::is_valid_boolean(false_value))
  
  // Test boolean string conversion
  let true_string = "true"
  match DataValidator::parse_boolean(true_string) {
    Some(true) => assert_true(true)
    _ => assert_true(false)
  }
  
  let false_string = "false"
  match DataValidator::parse_boolean(false_string) {
    Some(false) => assert_true(true)
    _ => assert_true(false)
  }
  
  let invalid_boolean_string = "maybe"
  match DataValidator::parse_boolean(invalid_boolean_string) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Array Data Validation
test "array data validation" {
  // Test valid array validation
  let valid_array = [1, 2, 3, 4, 5]
  assert_true(DataValidator::is_valid_array(valid_array))
  
  // Test empty array validation
  let empty_array = []
  assert_true(DataValidator::is_valid_array(empty_array))
  
  // Test array length validation
  let short_array = [1, 2, 3]
  assert_true(DataValidator::is_valid_array_length(short_array, 1, 5))
  
  let long_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  assert_false(DataValidator::is_valid_array_length(long_array, 1, 5))
  
  // Test array element validation
  let string_array = ["a", "b", "c"]
  assert_true(DataValidator::is_valid_string_array(string_array))
  
  let mixed_array = ["a", 2, true]
  assert_false(DataValidator::is_valid_string_array(mixed_array))
  
  // Test array uniqueness validation
  let unique_array = [1, 2, 3, 4, 5]
  assert_true(DataValidator::is_unique_array(unique_array))
  
  let duplicate_array = [1, 2, 3, 2, 1]
  assert_false(DataValidator::is_unique_array(duplicate_array))
}

// Test 5: Email Validation
test "email validation" {
  // Test valid email addresses
  let valid_email1 = "user@example.com"
  assert_true(DataValidator::is_valid_email(valid_email1))
  
  let valid_email2 = "user.name+tag@example.co.uk"
  assert_true(DataValidator::is_valid_email(valid_email2))
  
  // Test invalid email addresses
  let invalid_email1 = "user@.com"
  assert_false(DataValidator::is_valid_email(invalid_email1))
  
  let invalid_email2 = "user@example"
  assert_false(DataValidator::is_valid_email(invalid_email2))
  
  let invalid_email3 = "user.example.com"
  assert_false(DataValidator::is_valid_email(invalid_email3))
  
  let invalid_email4 = "@example.com"
  assert_false(DataValidator::is_valid_email(invalid_email4))
  
  let invalid_email5 = "user@"
  assert_false(DataValidator::is_valid_email(invalid_email5))
}

// Test 6: URL Validation
test "url validation" {
  // Test valid URLs
  let valid_url1 = "https://www.example.com"
  assert_true(DataValidator::is_valid_url(valid_url1))
  
  let valid_url2 = "http://example.com/path/to/resource?query=value#fragment"
  assert_true(DataValidator::is_valid_url(valid_url2))
  
  let valid_url3 = "ftp://ftp.example.com/files"
  assert_true(DataValidator::is_valid_url(valid_url3))
  
  // Test invalid URLs
  let invalid_url1 = "not-a-url"
  assert_false(DataValidator::is_valid_url(invalid_url1))
  
  let invalid_url2 = "http://"
  assert_false(DataValidator::is_valid_url(invalid_url2))
  
  let invalid_url3 = "://example.com"
  assert_false(DataValidator::is_valid_url(invalid_url3))
}

// Test 7: Date and Time Validation
test "date and time validation" {
  // Test valid date formats
  let valid_date1 = "2023-01-01"
  assert_true(DataValidator::is_valid_date(valid_date1))
  
  let valid_date2 = "01/01/2023"
  assert_true(DataValidator::is_valid_date(valid_date2))
  
  // Test invalid date formats
  let invalid_date1 = "2023-13-01"  // Invalid month
  assert_false(DataValidator::is_valid_date(invalid_date1))
  
  let invalid_date2 = "2023-02-30"  // Invalid day
  assert_false(DataValidator::is_valid_date(invalid_date2))
  
  let invalid_date3 = "not-a-date"
  assert_false(DataValidator::is_valid_date(invalid_date3))
  
  // Test valid time formats
  let valid_time1 = "12:30:45"
  assert_true(DataValidator::is_valid_time(valid_time1))
  
  let valid_time2 = "23:59:59"
  assert_true(DataValidator::is_valid_time(valid_time2))
  
  // Test invalid time formats
  let invalid_time1 = "24:00:00"  // Invalid hour
  assert_false(DataValidator::is_valid_time(invalid_time1))
  
  let invalid_time2 = "12:60:00"  // Invalid minute
  assert_false(DataValidator::is_valid_time(invalid_time2))
  
  let invalid_time3 = "not-a-time"
  assert_false(DataValidator::is_valid_time(invalid_time3))
}

// Test 8: JSON Validation
test "json validation" {
  // Test valid JSON
  let valid_json1 = "{\"key\": \"value\"}"
  assert_true(DataValidator::is_valid_json(valid_json1))
  
  let valid_json2 = "{\"array\": [1, 2, 3], \"nested\": {\"key\": \"value\"}}"
  assert_true(DataValidator::is_valid_json(valid_json2))
  
  let valid_json3 = "[]"
  assert_true(DataValidator::is_valid_json(valid_json3))
  
  // Test invalid JSON
  let invalid_json1 = "{\"key\": value}"  // Missing quotes around value
  assert_false(DataValidator::is_valid_json(invalid_json1))
  
  let invalid_json2 = "{\"key\": \"value\""  // Missing closing brace
  assert_false(DataValidator::is_valid_json(invalid_json2))
  
  let invalid_json3 = "not-json"
  assert_false(DataValidator::is_valid_json(invalid_json3))
}

// Test 9: Phone Number Validation
test "phone number validation" {
  // Test valid phone numbers
  let valid_phone1 = "+1-123-456-7890"
  assert_true(DataValidator::is_valid_phone_number(valid_phone1))
  
  let valid_phone2 = "(123) 456-7890"
  assert_true(DataValidator::is_valid_phone_number(valid_phone2))
  
  let valid_phone3 = "1234567890"
  assert_true(DataValidator::is_valid_phone_number(valid_phone3))
  
  // Test invalid phone numbers
  let invalid_phone1 = "123-456-789"  // Too short
  assert_false(DataValidator::is_valid_phone_number(invalid_phone1))
  
  let invalid_phone2 = "phone-number"  // Contains letters
  assert_false(DataValidator::is_valid_phone_number(invalid_phone2))
  
  let invalid_phone3 = "1234567890123456"  // Too long
  assert_false(DataValidator::is_valid_phone_number(invalid_phone3))
}

// Test 10: Credit Card Validation
test "credit card validation" {
  // Test valid credit card numbers (using test numbers)
  let valid_visa = "4111111111111111"
  assert_true(DataValidator::is_valid_credit_card(valid_visa))
  
  let valid_mastercard = "5555555555554444"
  assert_true(DataValidator::is_valid_credit_card(valid_mastercard))
  
  let valid_amex = "378282246310005"
  assert_true(DataValidator::is_valid_credit_card(valid_amex))
  
  // Test invalid credit card numbers
  let invalid_card1 = "4111111111111112"  // Invalid checksum
  assert_false(DataValidator::is_valid_credit_card(invalid_card1))
  
  let invalid_card2 = "1234567890123456"  // Invalid card number
  assert_false(DataValidator::is_valid_credit_card(invalid_card2))
  
  let invalid_card3 = "not-a-credit-card"  // Contains letters
  assert_false(DataValidator::is_valid_credit_card(invalid_card3))
  
  let invalid_card4 = "411111111111111"   // Too short
  assert_false(DataValidator::is_valid_credit_card(invalid_card4))
}