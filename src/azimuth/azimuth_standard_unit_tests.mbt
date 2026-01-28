// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试和实际应用场景

test "add_function_basic_test" {
  // 测试基本的加法功能
  let result = @azimuth.add(10, 25)
  @azimuth.assert_eq(35, result)
}

test "multiply_function_basic_test" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(7, 8)
  @azimuth.assert_eq(56, result)
}

test "greet_function_basic_test" {
  // 测试基本的问候功能
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "divide_with_ceil_basic_test" {
  // 测试基本的向上取整除法功能
  let result = @azimuth.divide_with_ceil(17, 5)
  @azimuth.assert_eq(4, result)
}

test "complex_calculation_test" {
  // 测试复杂计算场景
  let subtotal = @azimuth.multiply(15, 8)  // 15 * 8 = 120
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, 10), 100)  // 120 * 10 / 100 = 12
  let total = @azimuth.add(subtotal, tax)  // 120 + 12 = 132
  @azimuth.assert_eq(132, total)
}

test "negative_operations_test" {
  // 测试负数运算
  let result1 = @azimuth.add(-10, 5)
  let result2 = @azimuth.multiply(-3, 4)
  let result3 = @azimuth.divide_with_ceil(-15, 4)
  
  @azimuth.assert_eq(-5, result1)
  @azimuth.assert_eq(-12, result2)
  @azimuth.assert_eq(-3, result3)
}

test "zero_handling_test" {
  // 测试零的处理
  let result1 = @azimuth.multiply(25, 0)
  let result2 = @azimuth.add(0, 42)
  let result3 = @azimuth.divide_with_ceil(100, 0)
  
  @azimuth.assert_eq(0, result1)
  @azimuth.assert_eq(42, result2)
  @azimuth.assert_eq(0, result3)
}

test "packaging_calculation_test" {
  // 测试包装计算的实际应用
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
}

test "string_greeting_special_chars_test" {
  // 测试特殊字符的问候功能
  let result1 = @azimuth.greet("世界")
  let result2 = @azimuth.greet("")
  let result3 = @azimuth.greet("123")
  
  @azimuth.assert_eq_string("Hello, 世界!", result1)
  @azimuth.assert_eq_string("Hello, !", result2)
  @azimuth.assert_eq_string("Hello, 123!", result3)
}

test "mathematical_properties_test" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  let commutative_add = @azimuth.add(x, y)
  let reverse_add = @azimuth.add(y, x)
  @azimuth.assert_eq(commutative_add, reverse_add)
  
  // 乘法交换律
  let commutative_mul = @azimuth.multiply(x, y)
  let reverse_mul = @azimuth.multiply(y, x)
  @azimuth.assert_eq(commutative_mul, reverse_mul)
}