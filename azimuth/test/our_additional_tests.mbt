// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(1, multiply(1, 5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(6, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-1, divide_with_ceil(-5, 5))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = add(10, 5)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 3)
  assert_eq(10, c)  // ((10+5)*2)/3 = 30/3 = 10
  
  let x = multiply(3, 4)
  let y = add(x, 2)
  let z = divide_with_ceil(y, 5)
  assert_eq(3, z)  // ((3*4)+2)/5 = 14/5 = 3
}

test "mathematical_identities" {
  // 测试数学恒等式
  assert_eq(15, add(7, 8))
  assert_eq(56, multiply(7, 8))
  assert_eq(add(5, multiply(3, 4)), multiply(2, 8) + 1)  // 5+12 = 16+1
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))  // 2*(3+4) = 2*3 + 2*4
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, verylongname!", greet("verylongname"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, special@chars#!", greet("special@chars#"))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(2, divide_with_ceil(-3, -2))
  assert_eq(1, divide_with_ceil(-2, -2))
  assert_eq(0, divide_with_ceil(-1, -2))
}

test "real_world_scenarios" {
  // 测试真实世界应用场景
  // 计算需要的容器数量
  let items = 23
  let container_capacity = 5
  let containers_needed = divide_with_ceil(items, container_capacity)
  assert_eq(5, containers_needed)  // 需要5个容器来装23个物品
  
  // 计算总价
  let unit_price = 7
  let quantity = 8
  let total_cost = multiply(unit_price, quantity)
  assert_eq(56, total_cost)
  
  // 计算折扣后的价格
  let discount = 15
  let final_price = add(total_cost, -discount)
  assert_eq(41, final_price)
}