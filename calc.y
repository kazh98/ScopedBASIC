#
# $Id: calc.y,v 1.4 2005/11/20 13:29:32 aamine Exp $
#
# Very simple calculater.

class Calcp
  prechigh
    nonassoc UMINUS
    nonassoc UCSTRING
    left '*' '/'
    left '%'
    left '+' '-'
    left '&'
    left '<' LE '>' GE
    left EQ NE
    right '='
  preclow
rule
  target: exp
        | /* none */ { result = Node.new( "NOP", "NOP", nil, nil ) }

  exp: exp '+' exp { result = Node.new( "ADD", "OPR", val[0], val[2] ) }
     | exp '-' exp { result = Node.new( "SUB", "OPR", val[0], val[2] ) }
     | exp '*' exp { result = Node.new( "MUL", "OPR", val[0], val[2] ) }
     | exp '/' exp { result = Node.new( "DIV", "OPR", val[0], val[2] ) }
     | exp '%' exp { result = Node.new( "MOD", "OPR", val[0], val[2] ) }
     | exp '&' exp { result = Node.new( "CAT", "OPR", val[0], val[2] ) }
     | exp '<' exp { result = Node.new( "LT", "OPR", val[0], val[2] ) }
     | exp LE  exp { result = Node.new( "LE", "OPR", val[0], val[2] ) }
     | exp '>' exp { result = Node.new( "GT", "OPR", val[0], val[2] ) }
     | exp GE  exp { result = Node.new( "GE", "OPR", val[0], val[2] ) }
     | exp EQ exp { result = Node.new( "EQ", "OPR", val[0], val[2] ) }
     | exp NE exp { result = Node.new( "NE", "OPR", val[0], val[2] ) }
     | var '=' exp { result = Node.new( "ASSIGN", "OPR", val[0], val[2] ) }
     | '(' exp ')' { result = val[1] }
     | '-' trm =UMINUS { result = Node.new( "INV", "OPR", val[1], nil ) }
     | '&' exp =UCSTRING { result = Node.new( "CST", "OPR", val[1], nil ) }
     | exp '(' pms ')' { result = Node.new( val[0], "CAL", val[2], nil ) }
     | trm
  trm: var
     | NUMBER      { result = Node.new( val[0], "LD_", nil, nil ) }
     | FLOAT       { result = Node.new( val[0], "LF_", nil, nil ) }
     | STRING      { result = Node.new( formatString( val[0] ), "STR", nil, nil ) }
     | ELSE        { result = Node.new( "ELSE", "ELSE", nil, nil ) }
     | '^' '(' ags ')' '(' exp ')' { result = Node.new( val[2], "LMB", val[5], nil ) }
     | COND '(' cds ')' { result = Node.new( val[2], "CND", nil, nil ) }
     | PRINT '(' target ')' { result = Node.new( nil, "PNT", val[2], nil ) }
  cds:             { result = [ ] }
     | cnd         { result = [ val[0] ] }
     | cds cnd     { result = val[0] << val[1] }
  cnd: exp ':' '(' target ')' { result = Node.new( val[0], "CONDITION", val[3], nil ) }
  pms: prm         { result = Node.new( val[0], "PRM", nil, nil ) }
  prm:             { result = [ ] }
     | exp         { result = [ val[ 0 ] ] }
     | prm ',' exp { result = val[0] << val[2] }
  ags: arg         { result = Node.new( val[0], "ARG", nil, nil ) }
  arg:             { result = [ ] }
     | var         { result = [ val[0] ] }
     | arg ',' var { result = val[0] << val[2] }
  var: VARIABLE    { result = Node.new( val[0], "VAR", nil, nil ) }
end

---- header
# $Id: calc.y,v 1.4 2005/11/20 13:29:32 aamine Exp $

def getVariableValue( name, var = [ { } ] )
  var.reverse_each { |item|
    if item.has_key?(name)
      return item[name]
    end
  }
  return nil
end

def formatString( str )
  str.gsub!( /\\n/, "\n" )

  return str
end

class Closure
  def initialize( lmb, var )
    @lmb = lmb
    @var = var
  end

  def getLmb
    return @lmb
  end

  def run( var )
    nvar = @var.dup
    nvar << var
    return @lmb.getLeft.calc( nvar )
  end
end

class Node
  def initialize( page, type, left, right )
    @page = page
    @type = type
    @left = left
    @right = right
  end

  def getPage
    return @page
  end

  def getLeft
    return @left
  end

  def getRight
    return @right
  end

  def draw( indent = "", r = false )
    suffix = "  "
    if r
      suffix = "| "
    end

    case @type
      when "ELSE"
        return true
      when "LF_", "LD_"
        print indent + "+" + @page.to_s + " (" + @type + ")\n"
      when "ARG", "PRM", "CND"
        print indent + "+[LIST] (" + @type + ")\n"
        @page.each_with_index { |item, index|
          item.draw( indent + suffix, index + 1 < @page.size )
        }
        return
      when "LMB", "CAL"
        print indent + "+" + "lambda (" + @type + ")\n"
        @page.draw( indent + suffix, @left || @right )
      when "CONDITION"
        print indent + "+" + "[ITEM] (" + @type + ")\n"
        @page.draw( indent + suffix, @left || @right )
      when "PNT"
        print indent + "+[EMBED]" + " (" + @type + ")\n"
      else
        print indent + "+" + @page + " (" + @type + ")\n"
    end

    if @left
      @left.draw( indent + suffix, @right )
    elsif @right
      print indent + suffix + "+ (nil)\n"
    end

    if @right
      @right.draw( indent + suffix, false )
    end
  end

  def calc( var = [ { } ] )
    case @type
      when "OPR"
        case @page
          when "ASSIGN"
            var.reverse_each { |item|
              if item.has_key?(@left.getPage)
                return item[@left.getPage] = @right.calc(var)
              end
            }
            return var[var.size - 1][@left.getPage] = @right.calc(var)
          when "ADD"
            return @left.calc(var) + @right.calc(var)
          when "SUB"
            return @left.calc(var) - @right.calc(var)
          when "MUL"
            return @left.calc(var) * @right.calc(var)
          when "DIV"
            return @left.calc(var) / @right.calc(var)
          when "MOD"
            return @left.calc(var) % @right.calc(var)
          when "CAT"
            left = @left.calc(var)
            right = @right.calc(var)
            if left.class != String
              left = left.to_s
            end
            if right.class != String
              right = right.to_s
            end
            return left + right
          when "LT"
            return @left.calc(var) < @right.calc(var)
          when "LE"
            return @left.calc(var) <= @right.calc(var)
          when "GT"
            return @left.calc(var) > @right.calc(var)
          when "GE"
            return @left.calc(var) >= @right.calc(var)
          when "EQ"
            return @left.calc(var) == @right.calc(var)
          when "NE"
            return @left.calc(var) != @right.calc(var)
          when "INV"
            return -@left.calc(var)
          when "CST"
            res = @left.calc(var)
            if res.class == String
              return res
            else
              return res.to_s
            end
        end
      when "CND"
        @page.each { |item|
          if item.getPage.calc(var)
            return item.getLeft.calc(var)
          end
        }
      when "LMB"
        return Closure.new( self, var )
      when "CAL"
        lvar = { }
        closure = @page.calc( var )
        lmb = closure.getLmb
        @left.getPage.each_with_index { |item, index|
          lvar[ lmb.getPage.getPage[ index ].getPage ] = item.calc( var )
        }
        return closure.run( lvar )
      when "PNT"
        if @left.class == Closure
          print "#<#closure #f>\n"
        else
          print @left.calc( var )
        end
        return nil
      when "NOP"
        return nil
      when "VAR"
        return getVariableValue( @page, var )
      else
        return @page
    end
  end
end
---- inner
  def initialize
    @var = { }
  end

  def parse(str)
    @q = []
    until str.empty?
      case str
        when /\A\s+/
        when /\A;.*$/
        when /\A[0-9]+\.[0-9]*/
          @q.push [:FLOAT, $&.to_f]
        when /\A\d+/
          @q.push [:NUMBER, $&.to_i]
        when /\A[mM]od/
          @q.push ['%', $&]
        when /\Acond/
          @q.push [:COND, $&]
        when /\Aelse/
          @q.push [:ELSE, $&]
        when /\Aprint/
          @q.push [:PRINT, $&]
        when /\A[a-zA-Z_]+/
          @q.push [:VARIABLE, $&]
        when /\A"([^"]*)"/
          @q.push [:STRING, $1]
        when /\A<=/
          @q.push [:LE, $&]
        when /\A>=/
          @q.push [:GE, $&]
        when /\A==/
          @q.push [:EQ, $&]
        when /\A!=/
          @q.push [:NE, $&]
        when /\A.|\n/o
          s = $&
          @q.push [s, s]
      end
      str = $'
    end
    @q.push [false, '$end']
    do_parse
  end

  def next_token
    @q.shift
  end

---- footer

parser = Calcp.new
vartbl = [ { } ]
while true
  print 'alish> '
  str = gets.chop!
  break if /q/i =~ str
  begin
    cls = parser.parse(str)
    res = cls.calc( vartbl )
    if getVariableValue( "stree", vartbl ) != 0
      cls.draw
    end
    if getVariableValue( "echo", vartbl ) != 0
      if res.class == Closure
        print "#<#closure #f>\n"
      else
        p res
      end
    end
  rescue ParseError
    puts $!
  end
end

