// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加 10 个高质量测试用例

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
}

test "multiplication_basic_tests" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "greet_function_standard" {
  // 问候函数标准测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "divide_with_ceil_zero_division" {
  // 向上取整除零处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "subtract_function_tests" {
  // 减法函数测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
}

test "mathematical_commutative_property" {
  // 数学交换律测试
  let a = 8
  let b = 12
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let items = 25
  let capacity = 6
  let packages = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, packages)
  
  let total_capacity = @azimuth.multiply(packages, capacity)
  @azimuth.assert_eq(30, total_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 10
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(10, result)  // ceil(10*3/4) + 2 = 8 + 2 = 10
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}