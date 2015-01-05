require "strscan"

module Groupon

  Token = Struct.new(:type, :value)

  ##
  # Lexer for Calculator
  #
  class Lexer
    def initialize(expr)
      @scanner = StringScanner.new(expr)
    end

    def action
      yield 
    end

    def next_token
      return if @scanner.eos?
      until token = do_next_token or @scanner.eos?; end
      token
    end

  private
    def do_next_token
      case 
      when text = @scanner.scan(/[\x20\t\r\f]+/) then action {}
      when text = @scanner.scan(/[()]/) then action { Token.new(text, text) }
      when text = @scanner.scan(/[+\-*\/]/) then action { Token.new(text.to_sym, text.to_sym) }  
      when text = @scanner.scan(/\d+\.\d+/) then action { Token.new(:FLOAT, text.to_f) }
      when text = @scanner.scan(/\d+/) then action { Token.new(:INTEGER, text.to_i) }
      when text = @scanner.scan(/\w+/) then action { Token.new(:METHOD, text.to_sym) }
      else
        raise  ScanError, "can not match: '#{@scanner.string[@scanner.pos .. -1]}'"
      end
    end
  end

  ##
  # Calculator is recursive descent parser.
  # 
  # expr ::= expr + term
  #       |  expr - term 
  #       |  term
  #
  # term ::= term * factor
  #       |  term / factor
  #       |  factor
  #
  # factor ::= '(' expr ')'
  #         |  Method '(' expr ')'
  #         |  Integer
  #         |  Float
  #
  class Calculator
    @math_methods = Hash[ Math.methods(false).map {|m| [m, Math.method(m)]} ]

    def self.math_call(name, argument)
      raise "Unkown Math Method #{name}!" unless method = @math_methods[name]
      method.call(argument)
    end

    # evaluates the expression in string.
    def eval(exp)
      @lexer = Lexer.new(exp)   
      @lookahead = @lexer.next_token
      expr
    end  

  private
    def expr
      v = term
      v = v.send(@token.value, term) while match? :+ or match? :-
      v 
    end

    def term
      v = factor
      v = v.send(@token.value, factor) while match? :* or match? :/
      v 
    end

    def factor
      match_number || match_method_call || match_bracket_expr || (raise SyntaxError, "Expression Syntax Error!")
    end

    def match?(type)
      if @lookahead and @lookahead.type == type
        @token, @lookahead = @lookahead, @lexer.next_token
        true
      else
        false
      end
    end

    def expect(type)
      raise "Expected #{type}" unless match? type 
    end

    # match : float or integer
    def match_number
      @token.value if match? :FLOAT or match? :INTEGER
    end

    # match : Method '(' expr ')'
    def match_method_call
      if match? :METHOD
        method = @token.value
        expect('('); v = Calculator.math_call(method, expr); expect(')'); v
      end
    end

    # match : '(' expr ')'
    def match_bracket_expr
      if match? '('
        v = expr; expect(')'); v
      end
    end
  end # Calculator

end # Groupon
