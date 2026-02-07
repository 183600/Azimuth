// 标准 MoonBit 测试用例 - 10个核心功能测试
// 为 azimuth 项目添加的标准测试用例

test "add_function_basic" {
  // 基本加法功能测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "multiply_function_basic" {
  // 基本乘法功能测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "subtract_function_basic" {
  // 基本减法功能测试
  @azimuth.assert_eq(2, @azimuth.subtract(8, 6))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
}

test "divide_with_ceil_positive" {
  // 向上取整除法正数测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_zero_divisor" {
  // 除零情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
}

test "greet_function_standard" {
  // 问候函数标准测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_property" {
  // 数学交换律测试
  let a = 8
  let b = 12
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let items = 35
  let capacity = 8
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  let total_capacity = @azimuth.multiply(boxes, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
  
  @azimuth.assert_eq(5, boxes)
  @azimuth.assert_eq(40, total_capacity)
  @azimuth.assert_eq(5, remaining)
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}