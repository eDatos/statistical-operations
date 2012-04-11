package org.siemac.metamac.statistical.operations.web.client.instance.view.handlers;

import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface InstanceUiHandlers extends UiHandlers {

    void saveInstance(InstanceDto instanceDto);

    void populateInfSuppliersOrg(String schemeUri);
    void populateInfSuppliersConcept(String schemeUri);
    void populateStatisticalUnitConcepts(String schemeUri);

}
