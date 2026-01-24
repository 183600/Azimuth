// 标准 MoonBit 测试用例 - Azimuth 库
// 测试文件包含不超过10个测试用例，覆盖核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0时返回0
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串测试
}

test "mathematical_properties" {
  // 测试数学性质：交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))      // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "boundary_value_operations" {
  // 测试边界值运算
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_combined_operations" {
  // 测试复杂组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(multiply(3, 4), multiply(2, 5)), 3)
  assert_eq(8, result2)  // ceil((12 + 10)/3) = ceil(22/3) = 8
}

test "special_character_greeting" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "real_world_application" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 计算总价
  let price_per_item = 10
  let total_cost = multiply(total_items, price_per_item)
  assert_eq(250, total_cost)  // 25 * 10 = 250
}