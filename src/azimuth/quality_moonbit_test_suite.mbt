// 高质量 MoonBit 测试套件
// 包含8个专业测试用例，覆盖核心功能和边界情况

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  // 测试基本算术运算的正确性和数学性质
  
  let a = 15
  let b = 7
  
  // 加法运算验证
  assert_eq(22, add(a, b))
  assert_eq(8, subtract(a, b))
  
  // 乘法运算验证
  assert_eq(105, multiply(a, b))
  
  // 除法向上取整验证
  assert_eq(3, divide_with_ceil(a, b))
  assert_eq(2, divide_with_ceil(b, a))
  
  // 数学性质验证：交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  // 全面测试除法的各种边界条件
  
  // 正常除法情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 零值处理
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))  // 错误情况默认返回0
  assert_eq(0, divide_with_ceil(10, 0))  // 错误情况默认返回0
  
  // 负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "string_operations_validation" {
  // 字符串操作验证测试
  // 测试字符串相关函数的正确性
  
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  
  // 空格处理
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, multiple words!", greet("multiple words"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 模拟多步骤计算过程，验证函数组合的正确性
  
  let initial = 100
  let multiplier = 3
  let addend = 25
  let subtrahend = 15
  let divisor = 4
  
  // 计算流水线：((initial * multiplier) + addend - subtrahend) / divisor
  let step1 = multiply(initial, multiplier)  // 100 * 3 = 300
  let step2 = add(step1, addend)             // 300 + 25 = 325
  let step3 = subtract(step2, subtrahend)    // 325 - 15 = 310
  let result = divide_with_ceil(step3, divisor)  // ceil(310/4) = 78
  
  // 验证中间步骤
  assert_eq(300, step1)
  assert_eq(325, step2)
  assert_eq(310, step3)
  assert_eq(78, result)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  // 验证基本算术运算的数学定律和性质
  
  let x = 12
  let y = 8
  let z = 5
  
  // 加法性质
  assert_eq(add(x, y), add(y, x))  // 交换律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))  // 结合律
  assert_eq(add(x, 0), x)  // 单位元
  
  // 乘法性质
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))  // 结合律
  assert_eq(multiply(x, 1), x)  // 单位元
  assert_eq(multiply(x, 0), 0)  // 零元
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 减法性质
  assert_eq(add(subtract(x, y), y), x)  // 逆运算
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 系统性测试各种边界条件和极值情况
  
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值边界
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, subtract(1, 0))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 负单位边界
  assert_eq(0, add(-1, 1))
  assert_eq(-100, multiply(-1, 100))
  assert_eq(1, subtract(0, -1))
  assert_eq(-100, divide_with_ceil(-100, 1))
  
  // 大数边界
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(33334, divide_with_ceil(large_num, 3))
  assert_eq(0, subtract(large_num, large_num))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 模拟购物车计算的完整业务场景
  
  // 商品信息
  let item1_price = 45
  let item1_qty = 2
  let item2_price = 28
  let item2_qty = 3
  let item3_price = 15
  let item3_qty = 4
  
  // 计算各商品小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 45 * 2 = 90
  let subtotal2 = multiply(item2_price, item2_qty)  // 28 * 3 = 84
  let subtotal3 = multiply(item3_price, item3_qty)  // 15 * 4 = 60
  
  // 计算商品总计
  let items_total = add(add(subtotal1, subtotal2), subtotal3)  // 90 + 84 + 60 = 234
  
  // 计算税费（8%税率，向上取整）
  let tax_rate = 8
  let tax_amount = divide_with_ceil(multiply(items_total, tax_rate), 100)  // ceil(234 * 8 / 100) = 19
  
  // 计算最终总价
  let grand_total = add(items_total, tax_amount)  // 234 + 19 = 253
  
  // 计算包装箱数（每箱最多装10件）
  let total_items = add(add(item1_qty, item2_qty), item3_qty)  // 2 + 3 + 4 = 9
  let boxes_needed = divide_with_ceil(total_items, 10)  // ceil(9/10) = 1
  
  // 验证计算结果
  assert_eq(90, subtotal1)
  assert_eq(84, subtotal2)
  assert_eq(60, subtotal3)
  assert_eq(234, items_total)
  assert_eq(19, tax_amount)
  assert_eq(253, grand_total)
  assert_eq(9, total_items)
  assert_eq(1, boxes_needed)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试函数在异常情况下的行为
  
  // 除零错误处理
  let zero_div_result1 = divide_with_ceil(50, 0)
  let zero_div_result2 = divide_with_ceil(0, 0)
  let zero_div_result3 = divide_with_ceil(-50, 0)
  
  // 验证错误情况下的安全返回值
  assert_eq(0, zero_div_result1)
  assert_eq(0, zero_div_result2)
  assert_eq(0, zero_div_result3)
  
  // 大数运算稳定性
  let big_num1 = 50000
  let big_num2 = 50000
  let big_add = add(big_num1, big_num2)
  let big_mult = multiply(big_num1, 2)
  let big_div = divide_with_ceil(big_num1, 3)
  
  // 验证大数运算结果
  assert_eq(100000, big_add)
  assert_eq(100000, big_mult)
  assert_eq(16667, big_div)
  
  // 负数运算一致性
  let neg1 = -15
  let neg2 = 8
  let pos1 = 15
  let pos2 = -8
  
  // 验证负数运算的一致性
  assert_eq(add(neg1, neg2), add(pos1, pos2))
  assert_eq(multiply(neg1, pos2), multiply(pos1, neg2))
}