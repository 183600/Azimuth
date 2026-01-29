// 独立的MoonBit测试用例 - 10个高质量测试
// 这个文件包含标准的MoonBit测试语法，可以直接使用

// 测试1: 基本加法功能
test "basic_addition_functionality" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

// 测试2: 基本乘法功能
test "basic_multiplication_functionality" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

// 测试3: 基本问候功能
test "greeting_function_basic" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

// 测试4: 向上取整除法基本功能
test "ceil_division_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(10, 0))
}

// 测试5: 数学交换律性质
test "mathematical_commutative_property" {
  let x = 7
  let y = 13
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

// 测试6: 零元素性质
test "zero_element_properties" {
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(0, multiply(5, 0))
}

// 测试7: 复杂计算场景
test "complex_calculation_scenario" {
  let base = 10
  let multiplier = 3
  let divisor = 4
  let result = divide_with_ceil(multiply(base, multiplier), divisor)
  assert_eq(8, result)  // ceil(10*3/4) = 8
}

// 测试8: 负数处理
test "negative_number_handling" {
  assert_eq(-3, subtract(2, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 4))
}

// 测试9: Unicode字符支持
test "unicode_character_support" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

// 测试10: 真实世界应用场景
test "real_world_application" {
  // 场景：计算装盒问题
  let total_items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}