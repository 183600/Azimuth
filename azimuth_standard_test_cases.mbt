// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含10个实用测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-6, multiply(2, -3))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 6
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素特性
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形的面积和周长
  let length = 10
  let width = 4
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(28, perimeter)
  
  // 计算立方体体积
  let side = 3
  let volume = multiply(multiply(side, side), side)
  assert_eq(27, volume)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(10, multiply(-2, -5))
  assert_eq(-8, multiply(4, -2))
  assert_eq(0, add(-5, 5))
}

test "sequential_operations" {
  // 测试序列运算
  // 斐波那契数列计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 累加求和
  let sum = add(add(add(1, 2), 3), 4)  // 1+2+3+4 = 10
  assert_eq(10, sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物计算
  let price = 50
  let quantity = 3
  let tax = 15
  let total = add(multiply(price, quantity), tax)
  assert_eq(165, total)
  
  // 温度转换简化版 (F = C × 2 + 30)
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}

test "boolean_assertions" {
  // 测试布尔断言
  assert_true(true)
  assert_true(10 > 5)
  assert_true(0 == 0)
  assert_true(-5 < 0)
  
  assert_false(false)
  assert_false(3 > 10)
  assert_false(1 == 0)
  assert_false(-3 > 0)
}

test "divide_with_ceil_operations" {
  // 测试向上取整除法
  // 整除情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 需要向上取整
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
}