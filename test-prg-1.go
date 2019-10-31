package experiment

import (
        "crypto/md5"
        "encoding/json"
        "io/ioutil"
        "math"
        "math/rand"
        "strings"

)

type Experiment struct {
        Values map[string]interface{} `json:"values"`
        Name   string                 `json:"name"`
}

type Distribution struct {
        OverTerm interface{} `json:"over"`
        // TODO: start and end are to be implemented in the future.
        Experiments   []Experiment `json:"experiments"`
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

func LoadConfigFromFile(fn string) (*ExperimentManager, error) {
        d, err := ioutil.ReadFile(fn)
        if err != nil {
                return nil, err
        }
        return loadConfig(d)
}

func LoadConfigFromCloudStorage(addr string) {
        // TODO: The configuration will most like be on the cloud
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
        newKeys := newManager.experimentKeys()
        for k, _ := range e.experimentKeys() {
                if _, ok := newKeys[k]; !ok {
                        return fmt.Errorf("new configuration doesn't have key %v as in previous config.", k)
                }
        }

        e.Distributions = newManager.Distributions

        return nil
}

// GetExperimentInstance is called when we want to perform a new experiment, most likely when a new bidding request comes in.
// Each distribution can be "over" a particular value which determins when an experiment value of a particular request should
// be the same as a past request. For example, if the distribution is over "UUID", then every request with the same UUID should
// be assigned the same experiments. For now, only UUID is an option.
func (e *ExperimentManager) GetExperimentInstance(uuid string) ExperimentInstance {
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
                }

                // Determine what experiment we are running
                sum := float64(0)
                var p float64
                var i int
                for i, p = range dist.Probabilities {
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

func (e *ExperimentInstance) FetchMaxCampaignCPMFraction() (float64, bool) {
        for _, exp := range e.experiments() {
                v, ok := exp.Values["maxCampaignCPMFraction"]
                if ok {
                        return v.(float64), true
                }
        }
        return 0.0, false
}
func (e *ExperimentInstance) FetchAlphaInc() (float64, bool) {
        for _, exp := range e.experiments() {
                v, ok := exp.Values["alphaInc"]
                if ok {
                        return v.(float64), true
                }
        }
        return 0.0, false
}
func (e *ExperimentInstance) FetchRefCTR() (float64, bool) {
        for _, exp := range e.experiments() {
                v, ok := exp.Values["refCTR"]
                if ok {
                        return v.(float64), true
                }
        }
        return 0.0, false
}
func (e *ExperimentInstance) FetchAlphaDec() (float64, bool) {
        for _, exp := range e.experiments() {
                v, ok := exp.Values["alphaDec"]
                if ok {
                        return v.(float64), true
                }
        }
        return 0.0, false
}
func (e *ExperimentInstance) FetchMinCampaignCPMFraction() (float64, bool) {
        for _, exp := range e.experiments() {
                v, ok := exp.Values["minCampaignCPMFraction"]
                if ok {
                        return v.(float64), true
                }
        }
        return 0.0, false
}

func (e *ExperimentInstance) IsPiecewiseLinearExperiment() bool {
        for _, exp := range e.experiments() {
                if strings.HasPrefix(exp.Name, "PiecewiseLinearExperiment") {
                        return true
                }
        }
        return false
}
func (e *ExperimentInstance) IsPerfMinCTRExperiment() bool {
        for _, exp := range e.experiments() {
                if strings.HasPrefix(exp.Name, "PerfMinCTRExperiment") {
                        return true
                }
        }
        return false
}

func probIdx(d []byte) float64 {
        h := md5.New()
        h.Write(d)
        sum := h.Sum(nil)
        p := uint16(uint16(sum[0]) + (uint16(sum[1]) << 8))
        return float64(p) / float64(math.MaxInt16)
}