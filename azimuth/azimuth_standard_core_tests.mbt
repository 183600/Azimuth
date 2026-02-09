// Azimuth 标心测试用例 - 使用标准 MoonBit 测试语法
// 包含8个高质量测试用例，覆盖核心功能

test "core_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(60, 40))
}

test "core_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "core_divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "core_divide_with_ceil_negative" {
  // 测试向上取整除法负数处理
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "core_greet_functionality" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "core_subtract_functionality" {
  // 测试减法功能
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
}

test "core_mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "core_business_scenario" {
  // 测试业务场景计算
  let total_items = 23
  let items_per_package = 5
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(2, remaining_space)
}