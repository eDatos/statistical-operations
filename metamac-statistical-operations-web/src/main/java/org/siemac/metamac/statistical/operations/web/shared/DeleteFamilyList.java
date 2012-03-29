package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;

@GenDispatch(isSecure = false)
public class DeleteFamilyList {

    @In(1)
    List<Long> familyIds;

}
