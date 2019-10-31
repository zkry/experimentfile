package experiment

import (
        "crypto/md5"
        "encoding/json"
        "math"
        "fmt"
        "math/rand"
        "strings"
    "reflect"
)

type Experiment struct {
        Values map[string]interface{} `json:"values"`
        Name   string                 `json:"name"`
}

type Distribution struct {
        OverTerm interface{} `json:"over"`
        // TODO: start and end are to be implemented in the future.
        Experiments   []Experiment `json:"experiments"`
        Conditions []string `json:"conditions"`
        Probabilities []float64    `json:"probabilities"`
}

type ExperimentManager struct {
        Distributions []Distribution `json:"distributions"`
}

func loadConfig(data []byte) (*ExperimentManager, error) {
        var em ExperimentManager
        err := json.Unmarshal(data, &em)
        if err != nil {
                return nil, err
        }
        return &em, nil
}

func NewExperimentManager() *ExperimentManager {
        em, _ := loadConfig(initialData)
        return em
}

// ExperimentInstance is a slice of experiment names (in the same order as the distribution). With this information, an experiment
// instance can access it's particular values.
type ExperimentInstance struct {
        ChosenExperimentsIdx []int
        ParentManager        *ExperimentManager
}

func (e *ExperimentManager) ReloadConfiguration(data []byte) error {
        newManager, err := loadConfig(data)
        if err != nil {
                return fmt.Errorf("unable to reload config: %v", err)
        }

        // Compare for combatability. New configs keys must be a superset of
        // old configuration.
        if len(newManager.Distributions) != len(e.Distributions) {
                return fmt.Errorf("new configuration doesn't have the same number of distributions.")
        }

        for i := 0; i < len(newManager.Distributions); i++ {
                od := e.Distributions[i]
                nd := newManager.Distributions[i]
                // Each old distribution experiment should exist in new one
                for _, newExp := range nd.Experiments {
                        for _, oldExp := range od.Experiments {
                                if strings.HasPrefix(newExp.Name, strings.Split(oldExp.Name, ">")[0]) {
                                        for oldName, oldVal := range oldExp.Values {
                                                val, ok := newExp.Values[oldName]
                                                if !ok {
                                                        return fmt.Errorf("new configuration added a field that didn't prevously exist: %v", oldName)
                                                }
                                                if reflect.TypeOf(oldVal) != reflect.TypeOf(val) {
                                                        return fmt.Errorf("name %s has mismatched types: %v and %v", oldExp.Name, oldVal, val)
                                                }
                                        }
                                }
                        }
                }
        }

        e.Distributions = newManager.Distributions

        return nil
}

type Evaler interface {
        Eval(string) interface{}
}

type DefaultEvaler struct{}

func Eval(string) interface{} {
        return true
}

// GetExperimentInstance is called when we want to perform a new experiment, most likely when a new bidding request comes in.
// Each distribution can be "over" a particular value which determins when an experiment value of a particular request should
// be the same as a past request. For example, if the distribution is over "UUID", then every request with the same UUID should
// be assigned the same experiments. For now, only UUID is an option.
func (e *ExperimentManager) GetExperimentInstance(uuid string, evaler Evaler) ExperimentInstance {
        var chosenExperiments []int

        // Since there can be multiple distributions, we must itterate over every distribution,
        // selecting an experiment based on the "over" param and experiment probabilities.
        for _, dist := range e.Distributions {
                // Calculate percentage based on OverTerm from 0 to 100
                var probVal float64
                switch v := dist.OverTerm.(type) {
                case string:
                        switch v {
                        case "UUID":
                                probVal = probIdx([]byte(uuid))
                        case "NONE":
                                probVal = rand.Float64() * 100
                        default:
                                // Use default segmentatino
                        }
                default:
                        // Use default segmentation
                        probVal = probIdx([]byte(uuid))
                }

                // Determine what experiment we are running
                sum := float64(0)
                var i int
                for i := 0; i < len(dist.Probabilities); i++ {
                        p := dist.Probabilities[i]
                        condition := dist.Conditions[i]
                        if condition != "" {
                                pass, ok := evaler.Eval(condition).(bool)
                                if !pass || !ok {
                                        continue
                                }
                        }
                        if probVal < (p + sum) {
                                break
                        }
                        sum += p
                }
                chosenExperiments = append(chosenExperiments, i)
        }
        return ExperimentInstance{ChosenExperimentsIdx: chosenExperiments, ParentManager: e}
}

func (e *ExperimentManager) experimentKeys() map[string]struct{} {
        retKeys := make(map[string]struct{})
        for _, dst := range e.Distributions {
                for _, exp := range dst.Experiments {
                        for key, _ := range exp.Values {
                                retKeys[key] = struct{}{}
                        }
                }
        }
        return retKeys
}

func (e *ExperimentInstance) experiments() []Experiment {
        var retExps []Experiment
        for i, eidx := range e.ChosenExperimentsIdx {
                if eidx >= len(e.ParentManager.Distributions[i].Experiments) {
                        continue
                }
                retExps = append(retExps, e.ParentManager.Distributions[i].Experiments[eidx])
        }
        return retExps
}

func (e *ExperimentInstance) ExperimentNames() []string {
        var retNames []string
        for _, exp := range e.experiments() {
                retNames = append(retNames, exp.Name)
        }
        return retNames
}

type MyExperimentA struct {
	AlphaInc	float64	
	AlphaDec	float64	
}



func (e *ExperimentInstance) IsMyExperimentA () (*MyExperimentA, bool) {
        for _, exp := range e.experiments() {
                if strings.HasPrefix(exp.Name, "MyExperimentA") {
                        return &MyExperimentA {
				AlphaInc: exp.Values["alphaInc"].(float64),
				AlphaDec: exp.Values["alphaDec"].(float64),
                        }, true

                }
        }
    return nil, false
}


var initialData = []byte(`{"distributions":[{"probabilities":[10.0],"experiments":[{"values":{"alphaInc":15,"alphaDec":5},"name":"MyExperimentA"}],"conditions":[""],"over":false}]}`)

func probIdx(d []byte) float64 {
        h := md5.New()
        h.Write(d)
        sum := h.Sum(nil)
        p := uint16(uint16(sum[0]) + (uint16(sum[1]) << 8))
        return float64(p) / float64(math.MaxUint16) * 100
}