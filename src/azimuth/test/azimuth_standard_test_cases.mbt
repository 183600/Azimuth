// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含10个实用测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "greeting_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 6
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素特性
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形的面积和周长
  let length = 10
  let width = 4
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(28, perimeter)
  
  // 计算立方体体积
  let side = 3
  let volume = @azimuth.multiply(@azimuth.multiply(side, side), side)
  @azimuth.assert_eq(27, volume)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(10, @azimuth.multiply(-2, -5))
  @azimuth.assert_eq(-8, @azimuth.multiply(4, -2))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "sequential_operations" {
  // 测试序列运算
  // 斐波那契数列计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  
  // 累加求和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)  // 1+2+3+4 = 10
  @azimuth.assert_eq(10, sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物计算
  let price = 50
  let quantity = 3
  let tax = 15
  let total = @azimuth.add(@azimuth.multiply(price, quantity), tax)
  @azimuth.assert_eq(165, total)
  
  // 温度转换简化版 (F = C × 2 + 30)
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-5 < 0)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(3 > 10)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(-3 > 0)
}

test "divide_with_ceil_operations" {
  // 测试向上取整除法
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  
  // 需要向上取整
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  
  // 特殊情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}