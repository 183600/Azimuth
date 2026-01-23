// 标准 MoonBit 测试用例 - Azimuth 库
// 包含核心功能的全面测试

test "add_basic_functionality" {
  // 测试加法基本功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "greet_basic_functionality" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let complex_result = add(
    multiply(5, divide_with_ceil(17, 4)),
    multiply(3, divide_with_ceil(10, 3))
  )
  assert_eq(37, complex_result)  // 5*5 + 3*4 = 25 + 12 = 37
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 25
  let quantity = 4
  let shipping = 10
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(110, total)  // 25*4 + 10 = 110
  
  // 包装计算
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(23/6) = 4
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq_string("Hello, A!", greet("A"))
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéí!", greet("ñáéí"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}