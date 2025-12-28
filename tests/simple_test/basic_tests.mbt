// Basic Test Suite for Azimuth Telemetry System
// This file contains basic test cases to verify fundamental functionality

test "basic arithmetic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(20 / 4, 5)
}

test "boolean logic operations" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

test "string operations" {
  assert_eq("hello" + " world", "hello world")
  assert_eq("test".length(), 4)
  assert_eq("".length(), 0)
}

test "array operations" {
  let number_array = [1, 2, 3, 4, 5]
  assert_eq(number_array.length(), 5)
  assert_eq(number_array[0], 1)
  assert_eq(number_array[4], 5)
  
  let string_array = ["a", "b", "c"]
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[1], "b")
}

test "option type operations" {
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
  assert_true(some_value != None)
  assert_true(none_value != Some(0))
}

test "numeric comparisons" {
  assert_true(1 < 2)
  assert_true(2 <= 2)
  assert_true(3 > 2)
  assert_true(3 >= 3)
  assert_true(5 == 5)
  assert_true(5 != 4)
}

test "string comparisons" {
  assert_true("apple" < "banana")
  assert_true("hello" == "hello")
  assert_true("world" != "hello")
  assert_true("a" <= "a")
  assert_true("z" >= "a")
}

test "array indexing and slicing" {
  let numbers = [10, 20, 30, 40, 50]
  assert_eq(numbers[0], 10)
  assert_eq(numbers[2], 30)
  assert_eq(numbers[4], 50)
}

test "negative number operations" {
  assert_eq(-5 + 10, 5)
  assert_eq(-3 * 2, -6)
  assert_true(-10 < 0)
  assert_true(-1 > -5)
}