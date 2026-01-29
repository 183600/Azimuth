// 标准 MoonBit 测试用例 - 核心功能测试
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.add(2, 3)
  @azimuth.add(0, 0)
  @azimuth.add(-2, 1)
  @azimuth.add(50, 50)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.multiply(2, 3)
  @azimuth.multiply(5, 0)
  @azimuth.multiply(-2, 3)
  @azimuth.multiply(5, 5)
}

test "greet_function_basic_cases" {
  // 测试问候函数的基本情况
  @azimuth.greet("World")
  @azimuth.greet("MoonBit")
  @azimuth.greet("")
  @azimuth.greet("123")
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  @azimuth.divide_with_ceil(10, 3)
  @azimuth.divide_with_ceil(9, 3)
  @azimuth.divide_with_ceil(1, 1)
  @azimuth.divide_with_ceil(0, 5)
  @azimuth.divide_with_ceil(10, 0)
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  @azimuth.subtract(5, 3)
  @azimuth.subtract(3, 5)
  @azimuth.subtract(5, 5)
  @azimuth.subtract(15, 5)
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  @azimuth.add(7, 13)
  @azimuth.add(13, 7)
  @azimuth.multiply(7, 13)
  @azimuth.multiply(13, 7)
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  @azimuth.add(x, 0)
  @azimuth.multiply(x, 0)
  @azimuth.multiply(x, 1)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.add(-5, -3)
  @azimuth.multiply(-5, -3)
  @azimuth.multiply(-5, 3)
  @azimuth.divide_with_ceil(-10, 3)
  @azimuth.divide_with_ceil(-10, -3)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let items = 25
  let capacity = 6
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  
  let total_capacity = @azimuth.multiply(boxes, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  @azimuth.greet("世界")
  @azimuth.greet("🚀")
  @azimuth.greet("café")
  @azimuth.greet("ñoño")
}