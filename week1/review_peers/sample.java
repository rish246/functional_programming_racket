import java.util.ArrayList;
import java.util.List;

class Main {
    public static void main(String[] args) {
        List<Integer> vals = new ArrayList();
        vals.add(1);
        vals.add(3);
        vals.add(5);

        map(new Function(), vals);
        System.out.println(vals);


    }

    public static void map(Function f, List<Integer> values) {
        for(int i=0; i<values.size(); i++)
        {
            values.set(i, f.m(values.get(i)));
        }     
    
    }
}

class Function {
    public Integer m(Integer v) 
    {
        return v + 1;
    }
}