// 标准 MoonBit 测试用例集
// 使用标准 MoonBit 测试语法编写的简洁高效测试用例
// 覆盖核心功能和边界情况

test "add_function_basic_cases" {
  // 加法函数基础测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 减法函数边界测试
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
  assert_eq(-10, subtract(5, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(1, divide_with_ceil(1, 10))
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 模拟实际应用场景：购物车计算
  
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total = add(subtotal1, subtotal2)
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)
  let grand_total = add(total, tax)
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试各种边界条件
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 负单位值测试
  assert_eq(-1, add(-1, 0))
  assert_eq(-100, multiply(-1, 100))
  assert_eq(-100, divide_with_ceil(-100, 1))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  // 模拟简单算法的计算过程
  
  let n = 10
  
  // O(1) 常数时间操作
  let constant_result = add(5, 3)
  
  // O(n) 线性时间操作：求和 1+2+...+n
  let sum_n = divide_with_ceil(multiply(n, add(n, 1)), 2)
  
  // O(n²) 二次时间操作：矩阵乘法简化模拟
  let matrix_result = multiply(n, n)
  
  // 验证结果
  assert_eq(8, constant_result)
  assert_eq(55, sum_n)  // 1+2+...+10 = 55
  assert_eq(100, matrix_result)
}