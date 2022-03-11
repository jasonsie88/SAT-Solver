#include<bits/stdc++.h>
#include "parser.h"
using namespace std;
struct satsol{
    int n,m,remain,level;
    
    vector<vector<int>> clause; 
    vector<int> X;  
    vector<int> state; 
    vector<vector<set<vector<int>>>> pos; 
    vector<vector<int>> wait; 
    vector<vector<int>> unit; 
    vector<int> order; 
    vector<int> pre; 
    vector<int> vlevel; 
    int clevel; 
    vector<vector<int>> D;
    vector<vector<int>> I;
    vector<vector<vector<int>>> learning; 
    vector<int> vlearning; 
    clock_t begin,end,time_t;
    void init(vector<vector<int> > &v,int nt){
        vector<vector<int> > count;
        clause=v;
        n=nt;
        m=(int)v.size();
        remain=nt;
        X.resize(n+1);
        state.assign(n+1,0);
        pos.assign(n+1,{{},{}});
        wait.assign(m,{0,1});
        for(int i=0;i<m;i++){
            for(int j=0;j<2;j++){
                if(wait[i][j]<clause[i].size()){
                    int y=clause[i][wait[i][j]];
                    bool flag= (y>0) ? 1 : 0 ;
                    pos[abs(y)][flag].insert({i,j});
                }
            }
        }
        for(int i=0;i<clause.size();i++){
            if(clause[i].size()==1){
                unit.push_back({clause[i][0],i});
            }
        }
        for(int i=1;i<=n;i++){
            count.push_back({0,i});
        }
        for(vector<int> &v:clause){
            for(int a : v){
                count[abs(a)-1][0]++;
            }
        }
        sort(count.rbegin(),count.rend());
        order.clear();
        for(int i=0;i<n;i++){
            order.push_back(count[i][1]);
        }
        pre.resize(n+1);
        vlevel.resize(n+1);
        clevel=-1;
        level=0;
        D.push_back({-1,-1,-1});
        I.push_back({});
    }
    void unassign(int x){
        remain++;
        state[x]=0;
        for(vector<int> v:learning.back()){
            int &p=wait[v[0]][v[1]];
            if(p< clause[v[0]].size()){
            	 int y=clause[v[0]][p];
                bool flag= (y>0) ? 1 : 0 ;
                pos[abs(y)][flag].erase({v[0],v[1]}); 
            }
            p=v[2];
            int y=clause[v[0]][p];
            bool flag=(y>0) ? 1:0;
            pos[abs(y)][flag].insert({v[0],v[1]});
        }
        clevel--;
        learning.pop_back();
        vlearning.pop_back();
    }
    int assign(int x,int val,int s){
        int ans=-1;
        clevel++;
        remain--;
        learning.push_back({});
        vlearning.push_back(x);
        X[x]=val;
        state[x]=s;
        vlevel[x]=level;
        for(vector<int> v : pos[x][val^1]){
            int &i=wait[v[0]][v[1]];
            int &j=wait[v[0]][v[1]^1];
            int it=i;
            for(i=max(i,j);i<clause[v[0]].size();i++){
                int y=clause[v[0]][i];
                bool flag= (y>0) ? 1 : 0 ;
                if(i!=j && !(state[abs(y)]>0 && X[abs(y)]!=flag)){
                    break;
                }
            }
            learning.back().push_back({v[0],v[1],it,i});
            if(i<clause[v[0]].size()){
                int y=clause[v[0]][i];
                bool flag= (y>0) ? 1 : 0 ;
                pos[abs(y)][flag].insert({v[0],v[1]}); 
            }
            if(j>=clause[v[0]].size()){
                ans=v[0];
                continue;
            }
            if(state[abs(clause[v[0]][j])]==0  && i>=clause[v[0]].size() ){
                unit.push_back({clause[v[0]][j],v[0]});
            }
        }
        pos[x][val^1].clear();
        return ans;      
    }
    void check(){
    	for(int i=1;i<=n;i++){
    	    bool flag=(X[i] ? 1 : 0);
    	    cout<<flag<<" ";
    	}
    	cout<<endl;
    	bool sat=true;
    	for(int i=0;i<m;i++){
    	    bool valid=false;
    	    for(int j=0;j<clause[i].size();j++){
    	    	bool flag= (X[abs(clause[i][j])] != clause[i][j] < 0);
    	    	valid |= flag;
    	    }
    	    if(!valid){
    	        sat=false;
    	        cout<<"not valid\n";
    	    }
    	}
    	if(sat){
    	    cout<<"valid\n";
    	}
    }
    string solve(double num,double changetime){
        time_t=clock();
        stringstream ss;
        while(1){
            int error=-1;
            bool sat=true;
            while(!unit.empty()){
                int x=unit.back()[0];
                int pr=unit.back()[1];
                int val;
                if(x>0){
                    val=1;
                }else{
                    val=0;
                }
                unit.pop_back();
                if(state[abs(x)]!=0){
                    if(X[abs(x)]==val){
                        continue;
                    }else{
                    	sat=0;
                    	break;
                    }
                }
                I.back().push_back(abs(x));
                pre[abs(x)]=pr;
                error=assign(abs(x),val,2);
                if(error>-1){
                    set<int> cut(clause[error].begin(),clause[error].end());
                    set<int> ture;
                    for(int t:cut){
                    	if(vlevel[abs(t)]==level){
                    	    ture.insert(abs(t));
                    	}
                    }
                    for(auto it=I.back().rbegin();it!=I.back().rend() && ture.size()>1;it++){
                    	if(cut.find(-*it) != cut.end() || cut.find(*it)!=cut.end()){
                    	    cut.insert(clause[pre[abs(*it)]].begin(),clause[pre[abs(*it)]].end());
                    	    cut.erase(*it);
                    	    cut.erase(-*it);
                    	    for(int t : clause[pre[abs(*it)]]){
                    	    	if(vlevel[abs(t)]==level){
                    	    	    ture.insert(abs(t));
                    	    	}
                    	    }                    	    
                    	    ture.erase(abs(*it));
                    	}
                    }
                    if(cut.size() > max(n*(1-num),10.0)){
                    	while(D.size()>1 && D.back()[2]==1){
                    	    while(I.back().size()>0){
                    	    	unassign(I.back().back());
                    	    	I.back().pop_back();
                    	    }
                    	    unassign(D.back()[0]);
                    	    D.pop_back();
                    	    I.pop_back();
                    	    level--;
                    	}
                    	while(I.back().size()>0){
                    	    unassign(I.back().back());
                    	    I.back().pop_back();
                    	}
                    	unassign(D.back()[0]);
                    	D.back()[1]^=1;
                    	D.back()[2]=1;
                    	unit.clear();
                    	assign(D.back()[0],D.back()[1],1);
                    	continue;
                    }
                    int maxlevel=0;
                    for(int t:cut){
                    	if(abs(t)!=*ture.begin()){
                    	    maxlevel=max(maxlevel,vlevel[abs(t)]);
                    	}
                    }
                    while(D.size()>maxlevel+1){
                    	while(I.back().size()>0){
                    	    unassign(I.back().back());
                    	    I.back().pop_back();
                    	}
                    	unassign(D.back()[0]);
                    	D.pop_back();
                    	I.pop_back();
                    }
                    level=maxlevel;
                    m++;
                    clause.push_back({});
                    int uip;
                    for(int it:cut){
                    	clause.back().push_back(it);
                    	if(abs(it)==*ture.begin()){
                    	    uip=it;	
                    	}
                    }
                    wait.push_back({0,1});
                    set<int> used;
                    for(int i=0;i<=clevel;i++){
                    	int z=vlearning[i];
                    	used.insert(z);
                    	for(int j=0;j<2;j++){
                    	    int &p=wait.back()[j];
                    	    int &q=wait.back()[j^1];
                    	    if(p>=clause.back().size()){
                    	    	continue;
                    	    }
                    	    int y=clause.back()[p];
                    	    bool flag=(y>0)? 1 : 0 ;
                    	    if(X[z]==flag || abs(y)!=z ){
                    	    	continue;
                    	    }
                    	    int tp=p;
                    	    for(p=max(p,q);p<clause.back().size();p++){
                    	    	int y=clause.back()[p];
                    	    	bool flag=(y>0)? 1 : 0 ;
                    	    	if(!(used.find(abs(y))!=used.end() && X[abs(y)]!=flag) && p!=q){
                    	    	    break;
                    	    	}
                    	    }
                    	    learning[i].push_back({(int)clause.size()-1,j,tp,p});              
                    	}
                    }
                    for(int j=0;j<2;j++){
                    	int &p=wait.back()[j];
                    	if(p<clause.back().size()){
                    	    int y=clause.back()[p];
                    	    bool flag=(y>0)? 1 : 0 ;
                    	    pos[abs(y)][flag].insert({(int)clause.size()-1,j});
                    	}
                    }
                    unit.clear();
                    unit.push_back({uip,(int)clause.size()-1});
                    //printf("%f\n", (double)(clock() - time_t) / CLOCKS_PER_SEC);
                    if((double) (clock()-time_t)/CLOCKS_PER_SEC > changetime){
                    	return ss.str();
                    }
                }
            }
            if(!sat){
                cout<<"unsat\n";
                ss << "s UNSATISFIABLE\n";
                break;
            }
            //cout<<"rem: "<<remain<<endl;
            if(error > -1){
               //cout<<"error > -1\n";
               continue;
            }
            if(remain<=0){
                cout<<level<<endl;
                check();
            	 ss << "s SATISFIABLE\nv ";
            	 for(int i=1;i<=n;i++){
            	     if(X[i]){
            	     	ss<<i<<" ";
            	     }else{
            	     	ss<<-i<<" ";
            	     }
            	 }
            	 ss<<"0\n";
            	 break;
            }
            for(int i=0;i<n;i++){
            	if(!state[order[i]]){
            	    //cout<<"level "<<level<<endl;
            	    level++;
            	    int value=rand()%2;
            	    D.push_back({order[i],value,0});
            	    I.push_back({});
            	    assign(order[i],value,1);
            	    break;
            	}
            }
        }
        cout<<"finish\n";
        return ss.str();
    }
};
bool cmp(vector<int> a,vector<int> b){

   return a.size() > b.size();
}
int main(int argc,char **argv){
    srand((unsigned)time(NULL));
    int num=10;
    double changetime=10;
    double restart=rand()%(70-50+1)+50;
    cout<<restart<<endl;
    string ans;   
    string instr=argv[1];
    string outstr=instr.substr(0,instr.size()-3)+"sat";
    ofstream out(outstr);
    vector<vector<int>> C;
    //vvi C;
    int n;
    parse_DIMACS_CNF(C,n,instr.c_str());
    vector<satsol> satsols(num);
    clock_t begin=clock();
    clock_t t=begin;
    clock_t end;
    sort(C.begin(),C.end(),cmp);
    for(int i=0;i<num;i++){
        satsols[i].init(C,n);
    }
    for(int i=0;;i=(i+1)%num){
    	if(ans.size()!=0){
    	    break;
    	}
    	printf("%f\n",(clock()-t)/CLOCKS_PER_SEC);
        if((double)(clock()-t)/CLOCKS_PER_SEC > restart){
            cout<<"restart\n";
            printf("%f\n",(clock()-t)/CLOCKS_PER_SEC);            
            t=clock();          
            for(int j=0;j<num;j++){
                vector<vector<int> > c=satsols[i].clause;
                satsols[i]=satsol();
                satsols[i].init(c,n);
            }
        }
        ans=satsols[i].solve((double)i/num,changetime);
    	cout<<"return solution "<<i<<"\n";
    }
    end=clock();
    cout<<"time: "<<(double)(end-begin)/CLOCKS_PER_SEC<<"s\n";
    out<<ans;
    return 0;
}
