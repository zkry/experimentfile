![Experimentfile Logo](/docs/logo.png)

# Experimentfile

An Experimentfile allows for the quick definition of various configurations and a specification
of the traffic that they should get. Go code for working with this data is then generated anlong
with the JSON configuration of the values. Such values can be read in during the run-time of a 
Go program, allowing a complete decoupling of config/traffic from the Go code.

# I Specification

## Installation

1. [Download and install Racket](https://download.racket-lang.org/) which comes with DrRacket.
2. Open DrRacket. Click on *File* → *Install Package...*. Enter `https://github.com/zkry/experimentfile`

## Running an Experimentfile

- You can either run a file from the terminal or via IDE:
  - *Terminal* On your command line, type `racket <name-of-experimentfile>`.
  - *IDE* Open DrRacket, open an experiment file, then click on the Run button.

## The Experimentfile

The following is an example of an experiment file:

```
#lang experimentfile
package exp

experiment MyExperimentA:
  x := 1
  y := [1, 2, 3]
  z := [0 to 1 step 0.03]

experiment MyExperimentB:

distribution:
  10: MyExperimentA
  10: MyExperimentB when `customertype == 1`
```

Experimentfiles consist of two types of statements, one to define experiments and one to define traffic. To export a variable, use the *:=* operator. You can list as many variables as your experiment needs.

You can specify ranges of values via the *[...]* operator. In the example above `y` will take on the values of 1, 2, and 3 while z will take on the values of 0.0, 0.03, 0.06, ..., 0.99. Ranges by default will generate an experiment for each combination of values. So 34 experiments come from `z`, 3 come from `y`, totaling 102 experiments for MyExperimentA (probably more than we want).

Distributions have the interesting property that the traffic given to an experiment is distributed to all of its generated experiments evenly. This means that by saying we give 10 percent of the traffic to MyExperimentA, we are actually giving 10/102 (0.098) percent of the traffic to each experiment that came from MyExperimentA.

By adding a `when \`...\`` to a distribution caes, we can specify a precondition that will be checked before assigning the experiment. In the example above, ExperimentB will only ever get traffic if the customertype variable is equal to one. To see how this works, see below.

Note, we need to type `package exp` to tell the code generator what the package name the generated code should be. Here we're saying that the package is called `exp` and the other Go files in the same directory also have this package statement.

# II Implementation

Asusming that an experimentfile was created, ran, and the generated files (.go and .json) uploaded somewhere, we can use the generated Go code in our logic. Let's call the package where the generated code is located in `exp`.

```
em := exp.NewExperimentManager()
```

Calling the generated `NewExperimentManager` method returns a struct which knows how to generate experiments based off of the distribution and how to put them into proper Go structs.

You can make an experiment by calling the `GetExperimentInstance` method on the manager. This method takes two arguments, an ID that identifies the trial run, and a struct that has an `Eval(string) interface{}` method. This is used to process the `when` statements of an experiment file. Consider using the [gval](https://github.com/PaesslerAG/gval) library for creating your Go evaluator. You may skip this step by providing the DefaultEvaler from the generated code.

`GetExperimentInstance` will then return a struct which has methods to deterimine what experiment you're on and to obtain the experiment values. Consider the example below based on the *Experimentfile* above.

```
func main() {
	em := exp.NewExperimentManager()

	for i := 0; i < 100; i++ {
		ei := em.GetExperimentInstance(genUUID(i), exp.DefaultEvaler())
		
		if expa, ok := ei.IsMyExperimentA(); ok {
			fmt.Println("x = ", expa.X)
			fmt.Println("Y = ", expa.Y)
			fmt.Println("Z = ", expa.Z)
		}
		
		if _, ok := ei.IsMyExperimentB(); ok {
			fmt.Println("MyExperimentB is running")
		}
	}
}
```


The experiment manager also has the `ReloadConfiguration` method. You can pass in the contents of a newly generated JSON file and if the new file is compatible (see below), the experiment will update accordingly. You will get back an error if there was a compatibility problem.

## Compatibility Rules

When you generate a go and json file, run the Go code (as shown above), and want to reload a new configuration, there are casese where the new configuration will not be compatible with the current Go code that is running. Please consider the following cases to ensure that changes to an experimentfile will allow proper reloading.

1. You can add new experiments, new fields to experiments, and new experiments to distributions. Since the code that is running doesn't have any knowledge of the new experiments/fields, it won't be able to use them but all existing code will still work fine.
2. If you assign traffic to an experiment, that experiment **must** have at least all of the fields that it had previously. Immagine a case where MyExperimentA was running with the fields x, y, and z. If you delet the z field, the code running, when trying to access z, will not be able to retriev a value.
3. It is ok to remove fields of an experiment if you do not assign any traffic to that experiment. Since the experiment has no traffic, we can be sure that the code will not try to get a field that we don't have a value for anymore.
