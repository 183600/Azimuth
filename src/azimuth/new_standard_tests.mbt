// 标准 MoonBit 测试用例 - Azimuth 项目
// 测试覆盖核心功能：add, multiply, greet, divide_with_ceil

test "add_function_positive_numbers" {
  // 测试正数加法
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(100, azimuth::add(50, 50))
  azimuth::assert_eq(0, azimuth::add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(0, azimuth::add(-5, 5))
  azimuth::assert_eq(-10, azimuth::add(-15, 5))
}

test "multiply_function_basic" {
  // 测试基本乘法运算
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
  azimuth::assert_eq(6, azimuth::multiply(-2, -3))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "divide_with_ceil_positive_division" {
  // 测试正数向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(1, -3))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  azimuth::assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth::multiply(boxes_needed, items_per_box)
  azimuth::assert_eq(30, total_capacity)
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let initial = 10
  let step1 = azimuth::add(initial, 5)        // 15
  let step2 = azimuth::multiply(step1, 2)     // 30
  let step3 = azimuth::divide_with_ceil(step2, 8)  // 4
  let result = azimuth::add(step3, 1)         // 5
  azimuth::assert_eq(5, result)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符处理
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, café!", azimuth::greet("café"))
  azimuth::assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
}