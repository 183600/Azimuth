// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "int_boundary_operations" {
  // 测试整数边界运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试边界值的加法
  inspect(max_int + 0, content="2147483647")
  inspect(min_int + 0, content="-2147483648")
  
  // 测试边界值的乘法
  inspect(max_int * 1, content="2147483647")
  inspect(min_int * 1, content="-2147483648")
  
  // 测试零值运算
  inspect(0 + max_int, content="2147483647")
  inspect(0 * min_int, content="0")
  
  // 测试绝对值函数
  inspect(Int::abs(-100), content="100")
  inspect(Int::abs(100), content="100")
  inspect(Int::abs(0), content="0")
}

test "string_unicode_operations" {
  // 测试字符串 Unicode 操作
  let ascii_str = "Hello"
  let unicode_str = "你好"
  let mixed_str = "Hello你好"
  let emoji_str = "🚀🌟"
  
  // 测试字符串长度
  inspect(@string.length(ascii_str), content="5")
  inspect(@string.length(unicode_str), content="2")
  inspect(@string.length(mixed_str), content="7")
  
  // 测试字符串连接
  let combined = ascii_str + " " + unicode_str
  inspect(combined, content="\"Hello 你好\"")
  
  // 测试字符串比较
  inspect(ascii_str < unicode_str, content="true")
  inspect(mixed_str > ascii_str, content="true")
  
  // 测试空字符串
  let empty_str = ""
  inspect(@string.length(empty_str), content="0")
  inspect(empty_str + ascii_str, content="\"Hello\"")
}

test "boolean_logical_expressions" {
  // 测试布尔逻辑表达式
  let t = true
  let f = false
  
  // 基本逻辑运算
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(f && t, content="false")
  inspect(f && f, content="false")
  
  inspect(t || t, content="true")
  inspect(t || f, content="true")
  inspect(f || t, content="true")
  inspect(f || f, content="false")
  
  // 复杂逻辑表达式
  inspect((t && f) || (t || f), content="true")
  inspect((t || f) && (f || t), content="true")
  inspect((f && t) || (f && f), content="false")
  
  // 否定运算
  inspect(!t, content="false")
  inspect(!f, content="true")
  inspect(!(!t), content="true")
  
  // 布尔比较
  inspect(t == t, content="true")
  inspect(t != f, content="true")
  inspect(f == f, content="true")
}

test "option_type_comprehensive" {
  // 测试 Option 类型的综合操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let some_string : String? = Some("MoonBit")
  let none_string : String? = None
  
  // 测试 Some 值的提取
  let extracted_int = match some_value {
    Some(x) => x
    None => 0
  }
  inspect(extracted_int, content="42")
  
  let extracted_string = match some_string {
    Some(s) => s
    None => "default"
  }
  inspect(extracted_string, content="\"MoonBit\"")
  
  // 测试 None 值的默认处理
  let default_int = match none_value {
    Some(x) => x
    None => -1
  }
  inspect(default_int, content="-1")
  
  let default_string = match none_string {
    Some(s) => s
    None => "default"
  }
  inspect(default_string, content="\"default\"")
  
  // 测试 Option 的链式操作
  let chained = match some_value {
    Some(x) => Some(x * 2)
    None => None
  }
  let final_result = match chained {
    Some(y) => y + 10
    None => 0
  }
  inspect(final_result, content="94")
}

test "array_and_list_operations" {
  // 测试数组和列表操作
  let num_array = [1, 2, 3, 4, 5]
  let string_array = ["apple", "banana", "cherry"]
  
  // 测试数组长度和索引
  inspect(@array.length(num_array), content="5")
  inspect(num_array[0], content="1")
  inspect(num_array[4], content="5")
  inspect(@array.length(string_array), content="3")
  inspect(string_array[1], content="\"banana\"")
  
  // 测试数组运算
  let sum_first_three = num_array[0] + num_array[1] + num_array[2]
  inspect(sum_first_three, content="6")
  
  let product_last_two = num_array[3] * num_array[4]
  inspect(product_last_two, content="20")
  
  // 测试列表操作（使用数组模拟列表）
  let list_data = [10, 20, 30, 40]
  let head = list_data[0]
  let tail_sum = list_data[1] + list_data[2] + list_data[3]
  
  inspect(head, content="10")
  inspect(tail_sum, content="90")
  
  // 测试边界访问
  let single_element = [100]
  inspect(single_element[0], content="100")
  inspect(@array.length(single_element), content="1")
}

test "character_encoding_and_comparison" {
  // 测试字符编码和比较
  let char_a = 'a'
  let char_A = 'A'
  let char_emoji = '🚀'
  let char_digit = '5'
  let char_chinese = '中'
  
  // 测试字符比较
  inspect(char_a < char_A, content="false")
  inspect(char_A < char_a, content="true")
  inspect(char_digit < char_a, content="true")
  
  // 测试字符转换为字符串
  inspect(@char.to_string(char_a), content="\"a\"")
  inspect(@char.to_string(char_A), content="\"A\"")
  inspect(@char.to_string(char_digit), content="\"5\"")
  
  // 测试字符运算（模拟）
  let char_code_a = 97  // 'a' 的 ASCII 码
  let char_code_A = 65  // 'A' 的 ASCII 码
  let difference = char_code_a - char_code_A
  inspect(difference, content="32")
  
  // 测试字符边界情况
  let char_space = ' '
  let char_newline = '\n'
  inspect(@char.to_string(char_space), content="\" \"")
  
  // 测试字符在字符串中的使用
  let single_char_string = @char.to_string(char_chinese)
  inspect(@string.length(single_char_string), content="1")
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列: 2, 5, 8, 11, 14...
  let arithmetic_first = 2
  let arithmetic_diff = 3
  let arithmetic_term5 = arithmetic_first + arithmetic_diff * 4
  inspect(arithmetic_term5, content="14")
  
  // 等比数列: 3, 6, 12, 24, 48...
  let geometric_first = 3
  let geometric_ratio = 2
  let geometric_term4 = geometric_first * geometric_ratio * geometric_ratio * geometric_ratio
  inspect(geometric_term4, content="24")
  
  // 平方数列: 1, 4, 9, 16, 25...
  let square1 = 1 * 1
  let square3 = 3 * 3
  let square5 = 5 * 5
  let square_sum = square1 + square3 + square5
  inspect(square_sum, content="35")
  
  // 阶乘计算: 5! = 120
  let factorial_1 = 1
  let factorial_2 = factorial_1 * 2
  let factorial_3 = factorial_2 * 3
  let factorial_4 = factorial_3 * 4
  let factorial_5 = factorial_4 * 5
  inspect(factorial_5, content="120")
  
  // 斐波那契数列: 1, 1, 2, 3, 5, 8...
  let fib1 = 1
  let fib2 = 1
  let fib3 = fib1 + fib2
  let fib4 = fib2 + fib3
  let fib5 = fib3 + fib4
  inspect(fib5, content="5")
}

test "type_conversion_and_validation" {
  // 测试类型转换和验证
  let int_value = 42
  let bool_true = true
  let bool_false = false
  let string_value = "123"
  
  // 测试整数转字符串
  inspect(@int.to_string(int_value), content="\"42\"")
  inspect(@int.to_string(0), content="\"0\"")
  inspect(@int.to_string(-100), content="\"-100\"")
  
  // 测试布尔值转字符串
  inspect(@bool.to_string(bool_true), content="\"true\"")
  inspect(@bool.to_string(bool_false), content="\"false\"")
  
  // 测试数值比较和验证
  inspect(int_value > 0, content="true")
  inspect(int_value < 100, content="true")
  inspect(int_value == 42, content="true")
  inspect(int_value != 0, content="true")
  
  // 测试边界值验证
  let zero_value = 0
  let negative_value = -50
  inspect(zero_value >= 0, content="true")
  inspect(negative_value < 0, content="true")
  
  // 测试复合类型验证
  let positive_result = int_value > 0 && int_value < 100
  let negative_check = int_value < 0 || int_value > 1000
  inspect(positive_result, content="true")
  inspect(negative_check, content="false")
  
  // 测试条件表达式
  let conditional_result = if int_value > 50 { "large" } else { "small" }
  inspect(conditional_result, content="\"small\"")
  
  let conditional_result2 = if bool_true { int_value * 2 } else { 0 }
  inspect(conditional_result2, content="84")
}