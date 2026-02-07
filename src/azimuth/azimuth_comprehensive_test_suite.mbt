// Azimuth 综合测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法的各种情况
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
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

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 25)        // 325
  let step3 = subtract(step2, 50)   // 275
  let step4 = divide_with_ceil(step3, 10)  // 28
  
  assert_eq(step1, 300)
  assert_eq(step2, 325)
  assert_eq(step3, 275)
  assert_eq(step4, 28)
}

test "business_scenario_calculation" {
  // 测试业务场景计算
  let unit_price = 25
  let quantity = 8
  let discount = 15
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 应用折扣
  let discounted_price = subtract(subtotal, discount)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  
  // 计算总价
  let total = add(discounted_price, tax)
  
  assert_eq(subtotal, 200)
  assert_eq(discounted_price, 185)
  assert_eq(tax, 19)
  assert_eq(total, 204)
}

test "error_handling_and_boundaries" {
  // 测试错误处理和边界情况
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 5), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 100), 100)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(100, 1), 100)
  
  // 错误情况
  assert_eq(divide_with_ceil(50, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "algorithm_simulation" {
  // 测试算法模拟
  // 模拟斐波那契数列前几项的计算
  let fib0 = 0
  let fib1 = 1
  let fib2 = add(fib1, fib0)  // 1
  let fib3 = add(fib2, fib1)  // 2
  let fib4 = add(fib3, fib2)  // 3
  let fib5 = add(fib4, fib3)  // 5
  
  assert_eq(fib2, 1)
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  
  // 验证黄金比例近似
  let ratio = divide_with_ceil(multiply(fib5, 100), fib4)
  assert_eq(ratio, 167)  // 5/3 ≈ 1.67，接近黄金比例1.618
}