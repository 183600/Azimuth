// Enhanced Azimuth 测试用例
// 使用标准的 MoonBit 测试语法，专注于核心功能的深度测试

test "string_unicode_comprehensive" {
  // 综合Unicode字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 👋🏽!", greet("👋🏽"))
  assert_eq_string("Hello, 🚀🌙!", greet("🚀🌙"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "mathematical_associativity_laws" {
  // 数学结合律测试
  // 加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let left_assoc = add(add(a, b), c)
  let right_assoc = add(a, add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  let left_mul = multiply(multiply(a, b), c)
  let right_mul = multiply(a, multiply(b, c))
  assert_eq(left_mul, right_mul)
}

test "boundary_comprehensive_stress" {
  // 综合边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的各种组合
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(0, multiply(max_val, 0))
  
  // 最小值的各种组合
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(0, multiply(min_val, 0))
}

test "negative_number_specialized" {
  // 负数专项测试
  // 负数加法测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(5, add(-3, 8))
  assert_eq(0, add(-5, 5))
  
  // 负数乘法测试
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-5, 0))
  
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "zero_value_edge_cases" {
  // 零值边界情况测试
  // 零值加法
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
  assert_eq(0, add(0, 0))
  
  // 零值乘法
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(0, 0))
  
  // 零值除法
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_expression_evaluation" {
  // 复杂表达式计算测试
  // 嵌套算术表达式
  let expr1 = add(multiply(10, 5), multiply(3, 2))
  assert_eq(56, expr1)  // 10*5 + 3*2 = 50 + 6 = 56
  
  let expr2 = multiply(add(7, 3), add(4, 2))
  assert_eq(60, expr2)  // (7+3)*(4+2) = 10*6 = 60
  
  let expr3 = divide_with_ceil(add(multiply(4, 5), 3), 2)
  assert_eq(12, expr3)  // ceil((4*5 + 3)/2) = ceil(23/2) = 12
  
  // 多层嵌套
  let expr4 = add(multiply(divide_with_ceil(15, 4), 3), 5)
  assert_eq(17, expr4)  // ceil(15/4)*3 + 5 = 4*3 + 5 = 17
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价（含税）
  let item1 = 299
  let item2 = 399
  let item3 = 199
  let subtotal = add(add(item1, item2), item3)
  let tax = divide_with_ceil(subtotal, 10)  // 10%税
  let total = add(subtotal, tax)
  assert_eq(986, total)  // 897 + ceil(897/10) = 897 + 90 = 987
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 91
  let score4 = 89
  let total_score = add(add(add(score1, score2), score3), score4)
  let average = divide_with_ceil(total_score, 4)
  assert_eq(91, average)  // ceil(360/4) = ceil(90) = 90
  
  // 场景3：批量计算折扣价格
  let original_price = 1599
  let discount_rate = 3  // 1/3 = 33%折扣
  let discounted_price = divide_with_ceil(multiply(original_price, 2), 3)
  assert_eq(1066, discounted_price)  // ceil(1599*2/3) = ceil(3198/3) = 1066
}

test "overflow_protection_comprehensive" {
  // 溢出保护综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  let overflow_add1 = add(max_val, 1)
  let overflow_add2 = add(max_val, max_val)
  let overflow_add3 = add(min_val, -1)
  let overflow_add4 = add(min_val, min_val)
  
  assert_true(overflow_add1 == max_val || overflow_add1 == min_val)
  assert_true(overflow_add2 == max_val || overflow_add2 == min_val)
  assert_true(overflow_add3 == max_val || overflow_add3 == min_val)
  assert_true(overflow_add4 == max_val || overflow_add4 == min_val)
  
  // 乘法溢出测试
  let overflow_mul1 = multiply(max_val, 2)
  let overflow_mul2 = multiply(46341, 46341)  // sqrt(max_val) + 1
  let overflow_mul3 = multiply(min_val, -1)  // 特殊情况
  let overflow_mul4 = multiply(min_val, 2)
  
  assert_true(overflow_mul1 == max_val || overflow_mul1 == min_val)
  assert_true(overflow_mul2 == max_val || overflow_mul2 == min_val)
  assert_eq(min_val, overflow_mul3)  // 最小值乘以-1的特殊情况
  assert_true(overflow_mul4 == max_val || overflow_mul4 == min_val)
}

test "performance_critical_operations" {
  // 性能关键操作测试
  // 大量连续操作
  let base = 1000
  let result1 = add(add(add(base, 500), 250), 125)
  assert_eq(1875, result1)
  
  let result2 = multiply(multiply(multiply(base, 2), 3), 4)
  assert_eq(24000, result2)
  
  // 混合操作性能测试
  let result3 = add(multiply(divide_with_ceil(1000, 3), 2), 5)
  assert_eq(675, result3)  // ceil(1000/3)*2 + 5 = 334*2 + 5 = 673
  
  // 循环式操作模拟
  let accumulator = 0
  let acc1 = add(accumulator, 100)
  let acc2 = add(acc1, 200)
  let acc3 = add(acc2, 300)
  let acc4 = add(acc3, 400)
  assert_eq(1000, acc4)
}

test "function_chaining_composition" {
  // 函数链式调用和组合测试
  // 简单链式调用
  let chain1 = add(multiply(5, 3), 2)
  assert_eq(17, chain1)  // 5*3 + 2 = 17
  
  let chain2 = multiply(add(10, 5), 2)
  assert_eq(30, chain2)  // (10+5)*2 = 30
  
  let chain3 = divide_with_ceil(multiply(4, 7), 3)
  assert_eq(10, chain3)  // ceil(4*7/3) = ceil(28/3) = 10
  
  // 复杂链式调用
  let complex_chain = add(
    multiply(divide_with_ceil(25, 4), 3),
    multiply(add(5, 3), 2)
  )
  assert_eq(33, complex_chain)  // ceil(25/4)*3 + (5+3)*2 = 7*3 + 8*2 = 21 + 16 = 37
  
  // 函数组合测试
  let compose1 = multiply(add(multiply(2, 3), 4), 5)
  assert_eq(50, compose1)  // (2*3 + 4)*5 = (6 + 4)*5 = 10*5 = 50
  
  let compose2 = divide_with_ceil(add(multiply(6, 7), multiply(8, 9)), 10)
  assert_eq(12, compose2)  // ceil((6*7 + 8*9)/10) = ceil((42 + 72)/10) = ceil(114/10) = 12
}