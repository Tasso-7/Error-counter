#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <map>

using namespace std;
typedef long double decimal;

struct num{
    decimal val=0, err=0;
    num(){
        val = 0; err = 0;
    }

    num(decimal v, decimal e){
        val = v;
        err = e>0?e:-e;
    }

    num(string x){
        string v = "";
        string e = "";
        bool in_error = false;
        for(auto a: x){
            if(a == '\\'){
                in_error = true;
                continue;
            }
            if(in_error) e+=a;
            else v+=a;
        }
        val = stod(v);
        if(e != "")
            err = stod(e);
    }

    decimal rel(){
        return err/val;
    }

    num operator+(num other){
        return num (val+other.val, err+other.err);
    }
    num operator-(num other){
        return num (val-other.val, err+other.err);
    }
    num operator*(num other){
        decimal r = rel() + other.rel();
        decimal a = val*other.val;
        return num (a, a*r);
    }
    num operator/(num other){
        decimal r = rel() + other.rel();
        decimal a = val/other.val;
        return num (a, a*r);
    }

    num(decimal d) : val(d){
        err = 0;
    }

    
    num operator+=(num other){
        val += other.val;
        err += other.err;
        return num (val, err);
    }
    num operator-=(num other){
        val -= other.val;
        err += other.err;
        return num (val, err);
    }
    num operator*=(num other){
        decimal r = rel() + other.rel();
        val = val*other.val;
        err = val * r;
        return num (val, err);
    }
    num operator/=(num other){
        decimal r = rel() + other.rel();
        val = val/other.val;
        err = r*val;
        return num (val, err);
    }

    bool operator==(num other){
        pair<decimal, decimal> k1 = {val, err};
        pair<decimal, decimal> k2 = {other.val, other.err};
        if(k1 > k2) swap(k1,k2);
        return k1.first+k1.second > k2.first - k2.second;
    }

    void print(){
        cout << val << "\\" << err << endl;
    }
};


struct ExpressionEvaluator {
    using VarMap = std::map<std::string, num>;
    // Functions defined as name -> pair<list of parameter names, expression string>
    struct FunctionDef {
        std::vector<std::string> params;
        std::string body;
    };
    using FuncMap = std::map<std::string, FunctionDef>;

    static num evaluate(const std::string& expr, const VarMap& variables = {}, const FuncMap& functions = {}) {
        ExpressionEvaluator evaluator(expr, variables, functions);
        num result = evaluator.parseExpression();
        if (evaluator.current_token_type != TokenType::End) {
            throw std::runtime_error("Unexpected token at the end of expression");
        }
        return result;
    }
    
    enum class TokenType { Number, Identifier, Plus, Minus, Multiply, Divide, LParen, RParen, Comma, End };

    std::string src;
    size_t pos = 0;
    VarMap vars;
    FuncMap funcs;
    TokenType current_token_type = TokenType::End;
    std::string current_token_text;
    num current_token_val = num();

    ExpressionEvaluator(std::string s, VarMap v, FuncMap f) 
        : src(std::move(s)), vars(std::move(v)), funcs(std::move(f)) {
        nextToken();
    }

    void nextToken() {
        while (pos < src.size() && std::isspace(static_cast<unsigned char>(src[pos]))) {
            pos++;
        }
        if (pos >= src.size()) {
            current_token_type = TokenType::End;
            return;
        }

        char c = src[pos];
        if (c == '+') { pos++; current_token_type = TokenType::Plus; return; }
        if (c == '-') { pos++; current_token_type = TokenType::Minus; return; }
        if (c == '*') { pos++; current_token_type = TokenType::Multiply; return; }
        if (c == '/') { pos++; current_token_type = TokenType::Divide; return; }
        if (c == '(') { pos++; current_token_type = TokenType::LParen; return; }
        if (c == ')') { pos++; current_token_type = TokenType::RParen; return; }
        if (c == ',') { pos++; current_token_type = TokenType::Comma; return; }

        if (std::isdigit(static_cast<unsigned char>(c)) || c == '.' || c=='\\') {
            size_t start = pos;
            bool decimal_seen = false;
            bool error_seen = false;
            while (pos < src.size() && (std::isdigit(static_cast<unsigned char>(src[pos])) || src[pos] == '.' || src[pos]=='\\')) {
                if (src[pos] == '.') {
                    if (decimal_seen) throw std::runtime_error("Multiple decimals in number");
                    decimal_seen = true;
                }
                if (src[pos] == '\\') {
                    if (error_seen) throw std::runtime_error("Multiple errors in number");
                    decimal_seen = false;
                    error_seen = true;
                }
                pos++;
            }
            current_token_type = TokenType::Number;
            current_token_text = src.substr(start, pos - start);
            current_token_val = num(current_token_text);
            return;
        }

        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            size_t start = pos;
            while (pos < src.size() && (std::isalnum(static_cast<unsigned char>(src[pos])) || src[pos] == '_')) {
                pos++;
            }
            current_token_type = TokenType::Identifier;
            current_token_text = src.substr(start, pos - start);
            return;
        }

        throw std::runtime_error(std::string("Unknown character: ") + c);
    }

    num parseExpression() {
        num value = parseTerm();
        while (current_token_type == TokenType::Plus || current_token_type == TokenType::Minus) {
            TokenType op = current_token_type;
            nextToken();
            num right = parseTerm();
            if (op == TokenType::Plus) value += right;
            else value -= right;
        }
        return value;
    }

    num parseTerm() {
        num value = parseFactor();
        while (current_token_type == TokenType::Multiply || current_token_type == TokenType::Divide) {
            TokenType op = current_token_type;
            nextToken();
            num right = parseFactor();
            if (op == TokenType::Multiply) value *= right;
            else {
                if (right == num()) throw std::runtime_error("Division by zero");
                value /= right;
            }
        }
        return value;
    }

    num parseFactor() {
        if (current_token_type == TokenType::Plus) {
            nextToken();
            return parsePrimary();
        }
        if (current_token_type == TokenType::Minus) {
            nextToken();
            return num()-parsePrimary();
        }
        return parsePrimary();
    }

    num parsePrimary() {
        if (current_token_type == TokenType::Number) {
            num val = current_token_val;
            nextToken();
            return val;
        }
        if (current_token_type == TokenType::Identifier) {
            std::string name = current_token_text;
            nextToken();
            if (current_token_type == TokenType::LParen) {
                nextToken(); // consume '('
                std::vector<num> args;
                if (current_token_type != TokenType::RParen) {
                    args.push_back(parseExpression());
                    while (current_token_type == TokenType::Comma) {
                        nextToken(); // consume ','
                        args.push_back(parseExpression());
                    }
                }
                if (current_token_type != TokenType::RParen) {
                    throw std::runtime_error("Expected ')' after function arguments");
                }
                nextToken(); // consume ')'

                auto it = funcs.find(name);
                if (it == funcs.end()) {
                    throw std::runtime_error("Unknown function: " + name);
                }
                
                const auto& funcDef = it->second;
                if (args.size() != funcDef.params.size()) {
                    throw std::runtime_error("Function " + name + " expects " + 
                                               std::to_string(funcDef.params.size()) + 
                                               " arguments, got " + std::to_string(args.size()));
                }

                // Create a local scope extending current variables with function parameters
                VarMap local_vars = vars;
                for (size_t i = 0; i < funcDef.params.size(); ++i) {
                    local_vars[funcDef.params[i]] = args[i];
                }

                // Evaluate the function body using the local context
                return evaluate(funcDef.body, local_vars, funcs);
            } else {
                auto it = vars.find(name);
                if (it == vars.end()) {
                    throw std::runtime_error("Unknown variable: " + name);
                }
                return it->second;
            }
        }
        if (current_token_type == TokenType::LParen) {
            nextToken(); // consume '('
            num val = parseExpression();
            if (current_token_type != TokenType::RParen) {
                throw std::runtime_error("Expected ')'");
            }
            nextToken(); // consume ')'
            return val;
        }
        throw std::runtime_error("Unexpected token in expression");
    }
};

vector<string> split_at(string a, char c){
    vector<string> out = {""};
    for(auto x : a){
        if(x==c)out.push_back("");
        else *prev(out.end())+=x;
    }
    return out;
}

int main(){
    cout << "Write numbers as 67.69\\0.42 where the number after \\ is the error (if not specified 0 will be used)\n";
    cout << "Type $VAR to define a variable, $FUN to define a function $SAVE to save, $LOAD to load, or an expression\n";

    ExpressionEvaluator::VarMap variables = {};
    // User-defined functions: name -> { {parameter_names}, expression_string }
    ExpressionEvaluator::FuncMap functions = {};

    streambuf* IN = cin.rdbuf();
    streambuf* OUT= cout.rdbuf();
    ifstream INfile;//("ERROR_DATA.ed");
    ofstream OUTfile;//("ERROR_DATA.ed");
    bool silent = false;

    while(true){
        if(!silent){cout << ">";}
        string exp; cin >> exp;
        if(exp == "") continue;
        
        if(exp == "$VAR"){
            string  name, value;
            if(!silent){cout << "NAME:";} cin >> name;
            if(!silent){cout << "VALUE:";} cin >> value;
            variables[name] = ExpressionEvaluator::evaluate(value, variables, functions); 
        }else if(exp == "$FUN"){
            string  name, parameters, value;
            if(!silent){cout << "NAME:";} cin >> name;
            if(!silent){cout << "Parameters:";} cin >> parameters;
            if(!silent){cout << "VALUE:";} cin >> value;
            functions[name] = {split_at(parameters,','),    value}; 
        }else if(exp=="$EOF"){
            cin.rdbuf(IN);
            INfile.close();
            silent = false;
        }else if(exp == "$SAVE"){
            OUTfile = ofstream("ERROR_DATA.ed");
            OUTfile.clear();
            cout.rdbuf(OUTfile.rdbuf());
            
            // VARS

            for(auto it = variables.begin(); it != variables.end(); it++){
                cout << "$VAR\n" << it->first<< "\n";
                it->second.print();
            }
            // FUNCTIONS
            for(auto it = functions.begin(); it != functions.end(); it++){
                cout << "$FUN\n" << it->first << "\n";
                cout << it->second.params[0];
                for(int i = 1; i < it->second.params.size(); i++) cout << "," << it->second.params[i];
                cout << "\n" << it->second.body << "\n";
                cout << "$EOF" << endl;
            }
            cout.rdbuf(OUT);
            OUTfile.close();
        }else if(exp == "$LOAD"){
            INfile = ifstream("ERROR_DATA.ed");
            cin.rdbuf(INfile.rdbuf());
            silent = true;
        }else{
            ExpressionEvaluator::evaluate(exp, variables, functions).print();
        }
    }
    return 0;
}