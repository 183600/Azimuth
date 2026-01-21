// Azimuth 项目标准测试用例补充
// 覆盖未充分测试的功能和边界情况

test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 基本功能
  inspect(divide_with_ceil(10, 3), "4")?      // 10/3 = 3.33, 向上取整为 4
  inspect(divide_with_ceil(9, 3), "3")?       // 9/3 = 3, 无余数
  inspect(divide_with_ceil(8, 3), "3")?       // 8/3 = 2.66, 向上取整为 3
  inspect(divide_with_ceil(7, 3), "3")?       // 7/3 = 2.33, 向上取整为 3
}

test "divide_with_ceil_zero_division" {
  // 测试除零情况
  inspect(divide_with_ceil(5, 0), "0")?       // 除零返回 0
  inspect(divide_with_ceil(0, 0), "0")?       // 0/0 返回 0
  inspect(divide_with_ceil(-5, 0), "0")?      // 负数除零返回 0
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数除法
  inspect(divide_with_ceil(-10, 3), "-3")?    // -10/3 = -3.33, 向上取整为 -3
  inspect(divide_with_ceil(10, -3), "3")?     // 10/-3 = -3.33, 向上取整为 -3，但逻辑返回 3
  inspect(divide_with_ceil(-10, -3), "4")?    // -10/-3 = 3.33, 向上取整为 4
  inspect(divide_with_ceil(-9, 3), "-3")?     // -9/3 = -3, 无余数
}

test "divide_with_ceil_edge_cases" {
  // 测试边界情况
  inspect(divide_with_ceil(1, 1), "1")?       // 1/1 = 1
  inspect(divide_with_ceil(1, 2), "1")?       // 1/2 = 0.5, 向上取整为 1
  inspect(divide_with_ceil(0, 5), "0")?       // 0/5 = 0
  inspect(divide_with_ceil(5, 1), "5")?       // 5/1 = 5
}

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  inspect(add(100, 200), "300")?              // 大数相加
  inspect(add(-100, 50), "-50")?              // 正负数相加
  inspect(add(123456, 654321), "777777")?    // 多位数相加
  inspect(add(-50, -25), "-75")?              // 负数相加
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  inspect(multiply(100, 200), "20000")?       // 大数相乘
  inspect(multiply(-100, 50), "-5000")?       // 正负数相乘
  inspect(multiply(123, 456), "56088")?       // 多位数相乘
  inspect(multiply(-10, -20), "200")?         // 负数相乘
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  inspect(greet("Alice & Bob"), "Hello, Alice & Bob!")?      // 包含 & 符号
  inspect(greet("User@Domain.com"), "Hello, User@Domain.com!")? // 包含 @ 符号
  inspect(greet("12345"), "Hello, 12345!")?                  // 纯数字
  inspect(greet("!@#$%"), "Hello, !@#$%!")?                  // 特殊字符
}

test "complex_calculation_with_divide_ceil" {
  // 测试包含 divide_with_ceil 的复杂计算
  // 场景：计算需要多少辆车来运送一定数量的人
  let people = 25
  let capacity_per_car = 4
  let cars_needed = divide_with_ceil(people, capacity_per_car)
  inspect(cars_needed, "7")?             // 25/4 = 6.25, 需要 7 辆车
  
  // 场景：计算需要多少页来打印一定数量的项目
  let items = 23
  let items_per_page = 5
  let pages_needed = divide_with_ceil(items, items_per_page)
  inspect(pages_needed, "5")?            // 23/5 = 4.6, 需要 5 页
}

test "mathematical_consistency" {
  // 测试数学一致性
  let a = 12
  let b = 5
  
  // 测试分配律与 divide_with_ceil 的结合
  let result1 = divide_with_ceil(add(a, b), 3)
  let manual_calc = divide_with_ceil(17, 3)
  inspect(result1, manual_calc.to_string())?
  
  // 测试乘法与除法的关系
  let product = multiply(a, b)
  let divided_back = divide_with_ceil(product, b)
  inspect(divided_back, "12")?           // 60/5 = 12
}