// 标准MoonBit测试用例 - 针对azimuth库函数的综合测试
// 使用标准的MoonBit测试语法

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(7, 8), 56)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 50), 50)
}

test "division_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
}

test "greet_function_with_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let step1 = add(a, b)
  let result = multiply(step1, c)
  assert_eq(result, 16)
  
  // 计算 a * b + c
  let step2 = multiply(a, b)
  let result2 = add(step2, c)
  assert_eq(result2, 17)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 负数除法
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  
  // 大数运算
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(1000, 1000), 1000000)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "real_world_application_calculations" {
  // 实际应用场景计算
  // 计算购物总价
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  let subtotal = multiply(item_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(subtotal, 100)
  assert_eq(tax, 10)
  assert_eq(total, 110)
}

test "algorithm_simulation_fibonacci" {
  // 算法模拟：斐波那契数列
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)  // 1
  let f3 = add(f2, f1)  // 2
  let f4 = add(f3, f2)  // 3
  let f5 = add(f4, f3)  // 5
  let f6 = add(f5, f4)  // 8
  
  assert_eq(f2, 1)
  assert_eq(f3, 2)
  assert_eq(f4, 3)
  assert_eq(f5, 5)
  assert_eq(f6, 8)
}