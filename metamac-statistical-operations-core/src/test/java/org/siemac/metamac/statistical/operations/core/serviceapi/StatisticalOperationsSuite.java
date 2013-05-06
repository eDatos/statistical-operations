package org.siemac.metamac.statistical.operations.core.serviceapi;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDtoFieldsTest;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDtoFieldsTest;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDtoFieldsTest;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(Suite.class)
@Suite.SuiteClasses({SecurityStatisticalOperationsServiceFacadeTest.class, 
                        StatisticalOperationsBaseServiceTest.class, 
                        StatisticalOperationsListsServiceTest.class,
                        StatisticalOperationsServiceFacadeTest.class,
                        FamilyBaseDtoFieldsTest.class,
                        OperationBaseDtoFieldsTest.class,
                        InstanceBaseDtoFieldsTest.class})
public class StatisticalOperationsSuite {
}
