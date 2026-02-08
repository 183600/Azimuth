// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的核心功能测试

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_function_normal_cases" {
  // 测试向上取整除法函数的正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "divide_with_ceil_function_boundary_cases" {
  // 测试向上取整除法函数的边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 15)        // 315
  let step3 = subtract(step2, 5)    // 310
  let step4 = divide_with_ceil(step3, 4)  // 78
  
  assert_eq(300, step1)
  assert_eq(315, step2)
  assert_eq(310, step3)
  assert_eq(78, step4)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算场景
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = multiply(item1_price, item1_qty)  // 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 30
  let total = add(subtotal1, subtotal2)             // 105
  let tax = divide_with_ceil(multiply(total, 10), 100)  // 11
  let grand_total = add(total, tax)                 // 116
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}

test "boundary_comprehensive_test" {
  // 综合边界测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, subtract(1, 0))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 负值测试
  assert_eq(-2, add(-1, -1))
  assert_eq(25, subtract(0, -25))
}