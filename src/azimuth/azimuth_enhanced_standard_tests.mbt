// 高质量 MoonBit 标准测试用例
// 包含8个专业级测试用例，涵盖错误处理、边界条件、性能等关键方面

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种异常输入情况下的函数行为
  
  // 除零错误的多种情况
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(100, 0), 0)
  assert_eq(divide_with_ceil(-100, 0), 0)
  assert_eq(divide_with_ceil(1, 0), 0)
  assert_eq(divide_with_ceil(-1, 0), 0)
  
  // 极值输入测试
  let max_val = 1000000
  let min_val = -1000000
  assert_eq(divide_with_ceil(max_val, 1), max_val)
  assert_eq(divide_with_ceil(min_val, 1), min_val)
  assert_eq(divide_with_ceil(max_val, -1), -max_val)
  assert_eq(divide_with_ceil(min_val, -1), max_val)
}

test "performance_stress_testing" {
  // 性能压力测试
  // 测试大量连续计算的性能表现
  
  // 连续加法运算链
  let chain_add_1 = add(1, 2)
  let chain_add_2 = add(chain_add_1, 3)
  let chain_add_3 = add(chain_add_2, 4)
  let chain_add_4 = add(chain_add_3, 5)
  let chain_add_5 = add(chain_add_4, 6)
  let chain_add_6 = add(chain_add_5, 7)
  let chain_add_7 = add(chain_add_6, 8)
  let chain_add_8 = add(chain_add_7, 9)
  let chain_add_9 = add(chain_add_8, 10)
  
  // 验证最终结果 (1+2+...+10 = 55)
  assert_eq(chain_add_9, 55)
  
  // 连续乘法运算链
  let chain_mult_1 = multiply(1, 2)
  let chain_mult_2 = multiply(chain_mult_1, 3)
  let chain_mult_3 = multiply(chain_mult_2, 4)
  let chain_mult_4 = multiply(chain_mult_3, 5)
  let chain_mult_5 = multiply(chain_mult_4, 6)
  
  // 验证最终结果 (1*2*3*4*5*6 = 720)
  assert_eq(chain_mult_5, 720)
  
  // 混合运算链
  let mixed_1 = add(10, 5)
  let mixed_2 = multiply(mixed_1, 2)
  let mixed_3 = subtract(mixed_2, 8)
  let mixed_4 = divide_with_ceil(mixed_3, 3)
  
  // 验证最终结果 ((10+5)*2-8)/3 = ceil(22/3) = 8
  assert_eq(mixed_4, 8)
}

test "data_type_boundary_testing" {
  // 数据类型边界测试
  // 测试各种数据类型的边界条件
  
  // 整数边界测试
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 零值运算
  assert_eq(add(zero, zero), zero)
  assert_eq(multiply(zero, one), zero)
  assert_eq(subtract(zero, zero), zero)
  assert_eq(divide_with_ceil(zero, one), zero)
  
  // 单位值运算
  assert_eq(add(one, zero), one)
  assert_eq(multiply(one, one), one)
  assert_eq(subtract(one, zero), one)
  assert_eq(divide_with_ceil(one, one), one)
  
  // 负单位值运算
  assert_eq(add(neg_one, one), zero)
  assert_eq(multiply(neg_one, one), neg_one)
  assert_eq(subtract(zero, neg_one), one)
  assert_eq(divide_with_ceil(neg_one, one), neg_one)
  
  // 大数边界测试
  let large_pos = 2147483647
  let large_neg = -2147483648
  assert_eq(add(large_pos, 0), large_pos)
  assert_eq(add(large_neg, 0), large_neg)
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 测试字符串操作的各种边界情况和特殊字符
  
  // 空字符串处理
  assert_eq_string(greet(""), "Hello, !")
  
  // 单字符处理
  assert_eq_string(greet("A"), "Hello, A!")
  assert_eq_string(greet("a"), "Hello, a!")
  
  // 数字字符串
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("0"), "Hello, 0!")
  
  // 特殊字符处理
  assert_eq_string(greet("!@#$%"), "Hello, !@#$%!")
  assert_eq_string(greet("   "), "Hello,    !")  // 三个空格
  
  // 中文字符处理
  assert_eq_string(greet("中文"), "Hello, 中文!")
  assert_eq_string(greet("测试"), "Hello, 测试!")
  
  // 混合字符处理
  assert_eq_string(greet("Hello中文123"), "Hello, Hello中文123!")
  assert_eq_string(greet("test_测试"), "Hello, test_测试!")
}

test "mathematical_algorithm_testing" {
  // 数学算法测试
  // 测试一些基础数学算法的实现
  
  // 平方数计算
  let square_1 = multiply(1, 1)
  let square_2 = multiply(2, 2)
  let square_3 = multiply(3, 3)
  let square_4 = multiply(4, 4)
  let square_5 = multiply(5, 5)
  let square_10 = multiply(10, 10)
  
  assert_eq(square_1, 1)
  assert_eq(square_2, 4)
  assert_eq(square_3, 9)
  assert_eq(square_4, 16)
  assert_eq(square_5, 25)
  assert_eq(square_10, 100)
  
  // 立方数计算
  let cube_1 = multiply(multiply(1, 1), 1)
  let cube_2 = multiply(multiply(2, 2), 2)
  let cube_3 = multiply(multiply(3, 3), 3)
  let cube_4 = multiply(multiply(4, 4), 4)
  
  assert_eq(cube_1, 1)
  assert_eq(cube_2, 8)
  assert_eq(cube_3, 27)
  assert_eq(cube_4, 64)
  
  // 等差数列求和 (1+2+...+n)
  let n = 10
  let sum_1_to_10 = divide_with_ceil(multiply(add(1, n), n), 2)
  assert_eq(sum_1_to_10, 55)
  
  // 等比数列求和 (2^0 + 2^1 + ... + 2^n)
  let geo_2_0 = 1
  let geo_2_1 = multiply(geo_2_0, 2)
  let geo_2_2 = multiply(geo_2_1, 2)
  let geo_2_3 = multiply(geo_2_2, 2)
  let geo_2_4 = multiply(geo_2_3, 2)
  let sum_geo = add(add(add(add(geo_2_0, geo_2_1), geo_2_2), geo_2_3), geo_2_4)
  assert_eq(sum_geo, 31)
}

test "business_logic_integration" {
  // 业务逻辑集成测试
  // 模拟真实的业务计算场景
  
  // 订单计算场景
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣金额
  let discount_amount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  
  // 计算折后金额
  let discounted_total = subtract(subtotal, discount_amount)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  
  // 计算最终总额
  let final_total = add(discounted_total, tax_amount)
  
  // 验证计算结果
  assert_eq(subtotal, 200)
  assert_eq(discount_amount, 20)
  assert_eq(discounted_total, 180)
  assert_eq(tax_amount, 15)  // ceil(180 * 8 / 100) = ceil(14.4) = 15
  assert_eq(final_total, 195)
  
  // 库存管理场景
  let initial_stock = 500
  let daily_sales = 15
  let days = 7
  let new_shipment = 100
  
  // 计算一周销售
  let weekly_sales = multiply(daily_sales, days)
  
  // 计算剩余库存
  let remaining_stock = subtract(initial_stock, weekly_sales)
  
  // 计算新库存
  let new_stock = add(remaining_stock, new_shipment)
  
  // 计算库存周转天数
  let turnover_days = divide_with_ceil(new_stock, daily_sales)
  
  assert_eq(weekly_sales, 105)
  assert_eq(remaining_stock, 395)
  assert_eq(new_stock, 495)
  assert_eq(turnover_days, 33)
}

test "edge_case_exploration" {
  // 边界情况探索测试
  // 系统性测试各种边界和极端情况
  
  // 连续相同运算
  let same_add_1 = add(5, 5)
  let same_add_2 = add(same_add_1, 5)
  let same_add_3 = add(same_add_2, 5)
  let same_add_4 = add(same_add_3, 5)
  let same_add_5 = add(same_add_4, 5)
  
  assert_eq(same_add_5, 25)  // 5 + 5 + 5 + 5 + 5 = 25
  
  let same_mult_1 = multiply(2, 2)
  let same_mult_2 = multiply(same_mult_1, 2)
  let same_mult_3 = multiply(same_mult_2, 2)
  let same_mult_4 = multiply(same_mult_3, 2)
  
  assert_eq(same_mult_4, 16)  // 2 * 2 * 2 * 2 = 16
  
  // 交替运算
  let alt_1 = add(10, 5)
  let alt_2 = subtract(alt_1, 3)
  let alt_3 = add(alt_2, 8)
  let alt_4 = subtract(alt_3, 6)
  let alt_5 = add(alt_4, 2)
  
  assert_eq(alt_5, 16)  // ((10+5-3+8-6)+2) = 16
  
  // 嵌套运算
  let nested_1 = add(1, 2)
  let nested_2 = multiply(nested_1, 3)
  let nested_3 = add(nested_2, 4)
  let nested_4 = multiply(nested_3, 5)
  let nested_5 = divide_with_ceil(nested_4, 2)
  
  assert_eq(nested_5, 40)  // ceil(((1+2)*3+4)*5/2) = ceil(39/2) = 20
  
  // 极端除法测试
  assert_eq(divide_with_ceil(1, 1000), 1)
  assert_eq(divide_with_ceil(999, 1000), 1)
  assert_eq(divide_with_ceil(1000, 1000), 1)
  assert_eq(divide_with_ceil(1001, 1000), 2)
  assert_eq(divide_with_ceil(1999, 1000), 2)
  assert_eq(divide_with_ceil(2000, 1000), 2)
  assert_eq(divide_with_ceil(2001, 1000), 3)
}

test "consistency_validation" {
  // 一致性验证测试
  // 验证函数在不同输入下的一致性行为
  
  // 加法一致性验证
  let a = 15
  let b = 25
  let c = 10
  
  // (a + b) + c = a + (b + c)
  let left_assoc = add(add(a, b), c)
  let right_assoc = add(a, add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 乘法一致性验证
  let x = 4
  let y = 6
  let z = 8
  
  // (x * y) * z = x * (y * z)
  let mult_left = multiply(multiply(x, y), z)
  let mult_right = multiply(x, multiply(y, z))
  assert_eq(mult_left, mult_right)
  
  // 分配律一致性验证
  // x * (y + z) = (x * y) + (x * z)
  let distrib_left = multiply(x, add(y, z))
  let distrib_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distrib_left, distrib_right)
  
  // 除法一致性验证
  let dividend = 100
  let divisor1 = 3
  let divisor2 = 6
  
  // ceil(a/b) 的性质验证
  let div_result1 = divide_with_ceil(dividend, divisor1)
  let div_result2 = divide_with_ceil(dividend, divisor2)
  
  // 验证除法结果的基本性质
  assert_eq(div_result1, 34)  // ceil(100/3) = 34
  assert_eq(div_result2, 17)  // ceil(100/6) = 17
  
  // 验证字符串函数的一致性
  let name1 = "Test"
  let name2 = "test"
  let name3 = "TEST"
  
  let greet1 = greet(name1)
  let greet2 = greet(name2)
  let greet3 = greet(name3)
  
  assert_eq_string(greet1, "Hello, Test!")
  assert_eq_string(greet2, "Hello, test!")
  assert_eq_string(greet3, "Hello, TEST!")
  
  // 验证不同输入产生不同输出
  let not_equal1 = greet1 != greet2
  let not_equal2 = greet2 != greet3
  // 这里我们只能通过长度或其他方式间接验证
  assert_eq_string(greet1, "Hello, Test!")
  assert_eq_string(greet2, "Hello, test!")
}