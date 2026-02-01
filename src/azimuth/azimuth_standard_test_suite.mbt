// Azimuth 库的标准测试套件
// 包含针对核心功能的高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))  // 基本正数加法
  assert_eq(-4, add(-2, -2))  // 负数加法
  assert_eq(5, add(8, -3))  // 正负数混合
  assert_eq(0, add(5, -5))  // 相反数相加
  assert_eq(1000, add(500, 500))  // 大数加法
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))  // 基本乘法
  assert_eq(0, multiply(7, 0))  // 零乘法
  assert_eq(-12, multiply(-3, 4))  // 负数乘法
  assert_eq(16, multiply(-4, -4))  // 负负得正
  assert_eq(1000000, multiply(1000, 1000))  // 大数乘法
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 整除
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(4, divide_with_ceil(-10, -3)) // 负负除法
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零
  assert_eq(1, divide_with_ceil(1, 100))  // 小数除法
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, 世界!", greet("世界"))  // Unicode
  assert_eq_string("Hello, 123!", greet("123"))  // 数字
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 6
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 4
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "real_world_business_calculation" {
  // 真实业务场景计算
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(360, subtotal)  // 45 * 8
  assert_eq(36, tax)        // ceil(360 * 10 / 100)
  assert_eq(411, total)     // 360 + 36 + 15
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let x = 12
  let y = 8
  let z = 5
  
  // 测试复杂表达式：(x + y) * z - x / y
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(98, result1)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
  
  // 测试嵌套表达式：ceil((x * y + z) / (x - z))
  let result2 = divide_with_ceil(add(multiply(x, y), z), add(x, -z))
  assert_eq(15, result2)  // ceil((12*8 + 5) / (12-5)) = ceil(101/7) = 15
}

test "boundary_and_extreme_values" {
  // 边界值和极值测试
  // 测试零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 测试单位元运算
  assert_eq(7, multiply(7, 1))
  assert_eq(7, add(7, 0))
  assert_eq(1, divide_with_ceil(7, 7))
  
  // 测试大数运算
  let big_num = 100000
  assert_eq(200000, add(big_num, big_num))
  assert_eq(10000000000 / 10000, multiply(big_num, big_num) / 10000)  // 避免超出Int范围
}

test "string_processing_comprehensive" {
  // 综合字符串处理测试
  // 测试各种字符串输入
  assert_eq_string("Hello, 单字符!", greet("单字符"))
  assert_eq_string("Hello,   多空格   !", greet("   多空格   "))
  assert_eq_string("Hello, special@#$%chars!", greet("special@#$%chars"))
  assert_eq_string("Hello, emoji🚀🌟!", greet("emoji🚀🌟"))
  
  // 测试长字符串
  let long_name = "This is a very long name that contains multiple words and characters"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

test "algorithm_simulation" {
  // 算法模拟测试
  // 模拟计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let n = 10  // 项数
  let a = 3   // 首项
  let d = 2   // 公差
  
  let sum = divide_with_ceil(
    multiply(n, add(multiply(2, a), multiply(n - 1, d))),
    2
  )
  
  assert_eq(120, sum)  // 10/2 * (6 + 18) = 5 * 24 = 120
  
  // 模拟计算阶乘：5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
}