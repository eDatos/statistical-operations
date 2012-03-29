package org.siemac.metamac.gopestat.web.client.instance.view.handlers;

import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface InstanceUiHandlers extends UiHandlers {

    void saveInstance(InstanceDto instanceDto);

    void populateInfSuppliersOrg(String schemeUri);
    void populateInfSuppliersConcept(String schemeUri);
    void populateStatisticalUnitConcepts(String schemeUri);

}
