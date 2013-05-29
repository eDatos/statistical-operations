package org.siemac.metamac.statistical.operations.web.shared.external;

import java.io.Serializable;

public enum ConceptSchemeTypeEnum implements Serializable {
    GLOSSARY, TRANSVERSAL, OPERATION, ROLE, MEASURE;

    private ConceptSchemeTypeEnum() {
    }

    public String getName() {
        return name();
    }
}
